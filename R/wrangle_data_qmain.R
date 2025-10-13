# **************************
# Constructed series helpers for data_qmain
# **************************

#' Optionally Extend Master Dataset with AREMOS History
#'
#' Pulls 11Q4 AREMOS exports and splices longer histories onto the main
#' dataset when requested.
#'
#' @param data_qmain_xts xts object containing the UDAMAN pull.
#' @param dat_raw_dir Base directory for raw data files.
#'
#' @return xts object, potentially extended with archival data.
#' @keywords internal
extend_qmain_history <- function(data_qmain_xts, dat_raw_dir) {
  # load aremos data from the 11Q4 subfolder
  data_aremos_hist_xts <- c(
    "TOUR1",
    "TOUR2",
    "TOUR3",
    "JP1",
    "US1",
    "TAX1",
    "TAX2",
    "BEA1",
    "BLS1",
    "BLS2",
    "BLS3",
    "MISC1",
    "MISC2"
  ) %>%
    purrr::map(
      ~ readr::read_tsv(here::here(
        dat_raw_dir,
        "11Q4",
        stringr::str_glue(.x, ".TSV")
      ))
    ) %>%
    purrr::reduce(~ dplyr::full_join(.x, .y, by = c("DATE"))) %>%
    dplyr::mutate(
      time = lubridate::yq(.data$DATE),
      .before = "DATE",
      .keep = "unused"
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(., c("@" = "_", "OCUP%" = "OCUPP"))
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .,
        c("OCUPP" = "OCCUPPADJ", "TRMS" = "TRMSADJ")
      )
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # identify series in data_qmain_xts have a longer history in data_aremos_hist_xts
  ser_ext <- dplyr::full_join(
    tsbox::ts_summary(data_qmain_xts),
    tsbox::ts_summary(data_aremos_hist_xts),
    by = "id"
  ) %>%
    dplyr::filter(.data$start.x > .data$start.y)

  message(
    "The history of the following series will be extended: ",
    stringr::str_flatten(ser_ext$id, collapse = ", ")
  )

  # extend data_qmain_xts with the longer histories from aremos
  data_qmain_xts <- data_qmain_xts %>%
    fcutils::multi_chain(data_aremos_hist_xts, ser_ext$id)

  data_qmain_xts
}

#' Import Existing Forecasts
#'
#' Loads quarterly and annual existing forecasts,
#' and constructs quarterly pseudo-exogenous series required by QMOD.
#'
#' @param dat_raw_dir Base directory for raw existing forecasts
#' @param equations_qmod BIMETS equation bundle (used for the exogenous list).
#' @param data_qmod_xts xts object of the filtered QMOD dataset.
#' @param save_outputs logical indicating if data should be saved.
#'
#' @return List with `data_existing_fcst` (xts) and `exog_list` (character vector).
#' @keywords internal
import_existing_fcst <- function(
  dat_raw_dir,
  dat_prcsd_dir,
  equations_qmod,
  data_qmod_xts,
  save_outputs
) {
  # load quarterly existing forecasts
  data_existing_fcst_xts <- readr::read_tsv(here::here(
    dat_raw_dir,
    stringr::str_glue("QSOL1", ".TSV")
  )) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_glue("QSOL2", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::mutate(
      time = lubridate::yq(.data$DATE),
      .before = "DATE",
      .keep = "unused"
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, c("@" = "_", "OCUP%" = "OCUPP"))
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # load annual existing forecasts
  data_existing_fcst_A_xts <- readr::read_tsv(here::here(
    dat_raw_dir,
    stringr::str_glue("ASOL1", ".TSV")
  )) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_glue("ASOL2", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_glue("ASOL3", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::rename("time" = "DATE") %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .x,
        c("@" = "_", "OCUP%" = "OCUPP", "$" = "_A")
      ),
      .cols = -"time"
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # identify annual series that need to be disaggregated to quarterly
  A_only <- setdiff(
    names(data_existing_fcst_A_xts),
    names(data_existing_fcst_xts) %>%
      stringr::str_replace_all(c("$" = "_A"))
  ) %>%
    stringr::str_subset("DUM", negate = TRUE)

  # disaggregate annual series to quarterly
  A_to_Q_series <- fcutils::disagg(
    data_existing_fcst_A_xts[, A_only],
    conv_type = "uhero",
    target_freq = "quarter",
    pattern = NULL
  )

  # rename disaggregated series to match quarterly naming conventions
  colnames(A_to_Q_series) <- stringr::str_replace_all(
    colnames(A_to_Q_series),
    c("_A$" = "")
  )

  # get the names of nominal income series
  nominal_names <- A_to_Q_series %>%
    names() %>%
    stringr::str_subset("^Y") %>%
    stringr::str_subset("_R_", negate = TRUE)

  # create corresponding real income series names
  # all nominal names end with a 3-letter location code
  real_names <- stringr::str_replace(
    nominal_names,
    "_([[:alpha:]]{3}$)",
    "_R_\\1"
  )

  # combine quarterly existing forecasts with disaggregated annual nominal series
  data_existing_fcst_xts <- data_existing_fcst_xts %>%
    tsbox::ts_c(A_to_Q_series %>% tsbox::ts_pick(nominal_names))

  # construct real income series from nominal income and CPI
  data_existing_fcst_xts <- data_existing_fcst_xts %>%
    tsbox::ts_c(
      data_existing_fcst_xts %>%
        tsbox::ts_tbl() %>%
        tsbox::ts_wide() %>%
        dplyr::mutate(
          100 *
            dplyr::across(
              .cols = dplyr::all_of(nominal_names),
              .names = "{real_names}"
            ) /
            .data$CPI_HON
        ) %>%
        dplyr::select("time", dplyr::all_of(real_names)) %>%
        tsbox::ts_long() %>%
        tsbox::ts_xts()
    )

  # construct additional series required by QMOD
  data_existing_fcst_xts$GDP_R_RES <- data_existing_fcst_xts$GDP_R_US +
    data_existing_fcst_xts$GDP_R_JP / data_existing_fcst_xts$YXR_JP

  # construct current base-year CPI from existing CPI series
  if ("CPI_B_HON" %in% names(data_qmod_xts)) {
    data_existing_fcst_xts <- data_existing_fcst_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      dplyr::left_join(
        tsbox::ts_chain(
          data_qmod_xts %>% tsbox::ts_pick("CPI_B_HON") %>% tsbox::ts_na_omit(),
          data_existing_fcst_xts %>%
            tsbox::ts_pick("CPI_HON") %>%
            tsbox::ts_na_omit()
        ) %>%
          tsbox::ts_tbl() %>%
          dplyr::mutate(id = "CPI_B_HON") %>%
          tsbox::ts_wide(),
        by = "time"
      ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }

  # make a copy of the existing forecast data before subsetting to exog_list
  data_existing_fcst_all_xts <- data_existing_fcst_xts

  exog_list <- equations_qmod$vexog %>%
    stringr::str_subset("IIS_|SIS_|IQ|TREND|CONST|DUM|SEASON", negate = TRUE)

  message(
    "The model relies on the following exogenous series (excluding deterministic variables): ",
    stringr::str_flatten(exog_list, collapse = ", ")
  )

  # ensure the pseudo-exogenous series are available in data_qmod_xts and data_existing_fcst_xts
  missing_in_qmod <- setdiff(exog_list, names(data_qmod_xts))
  if (length(missing_in_qmod) > 0) {
    warning(
      "The following exogenous series are missing from data_qmain: ",
      stringr::str_flatten(missing_in_qmod, collapse = ", ")
    )
  }
  missing_in_fcst <- setdiff(exog_list, names(data_existing_fcst_xts))
  if (length(missing_in_fcst) > 0) {
    stop(
      "The following exogenous series are missing from data_existing_fcst: ",
      stringr::str_flatten(missing_in_fcst, collapse = ", ")
    )
  }

  # extend data_qmod_xts with the pseudo-exogenous series
  data_exog_ext_fcst_xts <- data_qmod_xts %>%
    fcutils::multi_chain(data_existing_fcst_xts, exog_list)

  # replace series in exog_list with forecast chained to history
  data_existing_fcst_xts <- data_existing_fcst_all_xts %>%
    tsbox::ts_tbl() %>%
    dplyr::filter(!(.data$id %in% exog_list)) %>%
    tsbox::ts_c(data_exog_ext_fcst_xts) %>%
    tsbox::ts_xts()

  if (isTRUE(save_outputs)) {
    # save the pseudo-exogenous paths alongside the main dataset
    saveRDS(
      data_existing_fcst_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_existing_fcst.RDS")
      )
    )

    data_existing_fcst_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_glue("data_existing_fcst.csv")
        )
      )
  }

  list(
    data_existing_fcst = data_existing_fcst_xts,
    exog_list = exog_list
  )
}

#' Enrich Master Quarterly Dataset
#'
#' Applies indicator adjustments, fills missing histories, and adds
#' constructed series required for the quarterly model.
#'
#' @param data_qmain_xts xts object produced by [make_data_qmain()].
#'
#' @return Modified xts object with additional constructed series.
#' @keywords internal
wrangle_data_qmain <- function(data_qmain_xts) {
  stopifnot(!missing(data_qmain_xts))

  # outline: create helper lists of mnemonic-location combinations, backfill pandemic holes,
  # compute price ratios/shares, and ensure select series have smooth historical coverage

  pprm <- c("PPRM")
  ocupp <- c("OCUPPADJ")
  vplant <- c("TRMSADJ")
  vadc <- c("VADC")
  vadccrair <- c("VADCCRAIR")
  vloscrair <- c("VLOSCRAIR")
  viscrair <- c("VISCRAIR")

  sh_vadccrair <- c("SH_VADCCRAIR")
  sh_oc <- c("SH_OC")
  sh_vplant <- c("SH_TRMSADJ")

  loc_list <- c("HI", "HON", "HAW", "MAU", "KAU")
  loc1_list <- c("HI")

  bulk_mnem_geo <- function(prefix, geos) {
    as.vector(outer(prefix, geos, FUN = function(x, y) paste(x, y, sep = "_")))
  }

  ocupp_list <- bulk_mnem_geo(ocupp, loc_list)
  pprm_list <- bulk_mnem_geo(pprm, loc_list)
  vplant_list <- bulk_mnem_geo(vplant, loc_list)
  vadc_list <- bulk_mnem_geo(vadc, loc_list)
  vadccrair_list <- bulk_mnem_geo(vadccrair, loc_list)
  vloscrair_list <- bulk_mnem_geo(vloscrair, loc1_list)
  viscrair_list <- bulk_mnem_geo(viscrair, loc1_list)

  sh_vadccrair_list <- bulk_mnem_geo(sh_vadccrair, loc_list)
  sh_oc_list <- bulk_mnem_geo(sh_oc, loc_list)
  sh_vplant_list <- bulk_mnem_geo(sh_vplant, loc_list)

  data_qmain_xts[
    fcutils::p(
      fcutils::find_start(data_qmain_xts$VADC_HI),
      fcutils::find_start(data_qmain_xts$VADCCRAIR_HI)
    ),
    vadccrair_list
  ] <- 0
  data_qmain_xts[
    fcutils::p(
      fcutils::find_start(data_qmain_xts$VLOS_HI),
      fcutils::find_start(data_qmain_xts$VLOSCRAIR_HI)
    ),
    vloscrair_list
  ] <- 0
  data_qmain_xts[
    fcutils::p(
      fcutils::find_start(data_qmain_xts$VIS_HI),
      fcutils::find_start(data_qmain_xts$VISCRAIR_HI)
    ),
    viscrair_list
  ] <- 0

  # zero out pandemic-era observations and single-point outliers so downstream averages behave
  data_qmain_xts[fcutils::pq(2020.2, 2022.1), vadccrair_list] <- 0
  data_qmain_xts[fcutils::pq(2020.2, 2022.1), vloscrair_list] <- 0
  data_qmain_xts[fcutils::pq(2020.2, 2022.1), viscrair_list] <- 0
  data_qmain_xts$VLOSJP_KAU[fcutils::pq(2020.2, 2021.3)] <- 0
  data_qmain_xts$VLOSJP_MAU[fcutils::pq(2020.2, 2021.1)] <- 0
  data_qmain_xts$VLOSJP_HAW[fcutils::pq(2020.2, 2021.1)] <- 0
  data_qmain_xts$VLOSJP_HON[fcutils::pq(2020.2, 2020.2)] <- 0
  data_qmain_xts$VLOSJP_HI[fcutils::pq(2020.2, 2020.2)] <- 0
  data_qmain_xts$VLOSRES_MAU[fcutils::pq(2001.4, 2001.4)] <- 0

  data_qmain_xts$VEXP_HI <- data_qmain_xts$VEXP_HI %>%
    fcutils::interpol(data_qmain_xts$VADC_HI)
  data_qmain_xts$VEXP_R_HI <- data_qmain_xts$VEXP_R_HI %>%
    fcutils::interpol(data_qmain_xts$VADC_HI)
  data_qmain_xts$VEXP_RB_HI <- data_qmain_xts$VEXP_RB_HI %>%
    fcutils::interpol(data_qmain_xts$VADC_HI)
  data_qmain_xts$VEXP_HON <- data_qmain_xts$VEXP_HON %>%
    fcutils::interpol(data_qmain_xts$VADC_HON)
  data_qmain_xts$YPJ_SV_R_HI <- data_qmain_xts$YPJ_SV_R_HI %>%
    fcutils::interpol(data_qmain_xts$YPJ_R_HI)

  # convert to wide form so we can compute price ratios and geographic shares
  data_qmain_xts <- data_qmain_xts %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_wide() %>%
    dplyr::mutate(
      (dplyr::across(
        .cols = dplyr::all_of(vadc_list),
        .names = "{pprm_list}"
      ) -
        dplyr::pick(dplyr::all_of(vadccrair_list)) * 1 / 7) /
        (dplyr::pick(dplyr::all_of(vplant_list)) *
          dplyr::pick(dplyr::all_of(ocupp_list)) /
          100)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(ocupp_list),
        .fns = ~ .x / OCUPPADJ_HI,
        .names = "{sh_oc_list}"
      ),
      dplyr::across(
        .cols = dplyr::all_of(vplant_list),
        .fns = ~ .x / TRMSADJ_HI,
        .names = "{sh_vplant_list}"
      ),
      dplyr::across(
        .cols = dplyr::all_of(vadccrair_list),
        .fns = ~ .x / VADCCRAIR_HI,
        .names = "{sh_vadccrair_list}"
      )
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # fill small gaps in agriculture and income series so later transformations have no holes
  smpl_aginc <- fcutils::p("2022-04-01", "2023-01-01")
  aglist <- c(
    "EAG_HI",
    "EAG_NBI",
    "YPJAG_R_HI",
    "YPJAGFA_R_HI",
    "E_HI",
    "E_NBI"
  )

  data_qmain_xts[smpl_aginc, aglist] <- zoo::na.approx(data_qmain_xts[
    smpl_aginc,
    aglist
  ])

  smpl_ylinc <- fcutils::p("2020-10-01", "2022-01-01")
  yllist <- c(
    "YLAG_R_HI",
    "YLAG_HI",
    "YPJAG_R_HI",
    "YL_ELSE_R_HI",
    "YL_ELSE_HI",
    "YPJ_ELSE_R_HI",
    "YLMN_R_HI",
    "YLMN_HI",
    "YPJMN_R_HI"
  )

  data_qmain_xts[smpl_ylinc, yllist] <- zoo::na.approx(data_qmain_xts[
    smpl_ylinc,
    yllist
  ])

  data_qmain_xts$YLAG_R_NBI <- data_qmain_xts$YLAG_R_HI -
    data_qmain_xts$YLAG_R_HON
  data_qmain_xts$YPJAG_R_NBI <- data_qmain_xts$YLAG_R_NBI /
    data_qmain_xts$EAG_NBI
  data_qmain_xts$YLAG_NBI <- data_qmain_xts$YLAG_HI -
    data_qmain_xts$YLAG_HON

  data_qmain_xts$YL_SV_HI <- data_qmain_xts$YLHC_HI +
    data_qmain_xts$YLAF_HI +
    data_qmain_xts$YL_ELSE_HI
  data_qmain_xts$YL_SV_R_HI <- data_qmain_xts$YLHC_R_HI +
    data_qmain_xts$YLAF_R_HI +
    data_qmain_xts$YL_ELSE_R_HI

  data_qmain_xts$GDP_R_RES <- data_qmain_xts$GDP_R_US +
    data_qmain_xts$GDP_R_JP / data_qmain_xts$YXR_JP

  data_qmain_xts$LFPR_HI <- data_qmain_xts$LF_HI / data_qmain_xts$NR_HI
  data_qmain_xts$LFPR_HON <- data_qmain_xts$LF_HON / data_qmain_xts$NR_HON
  data_qmain_xts$LFPR_NBI <- data_qmain_xts$LF_NBI / data_qmain_xts$NR_NBI

  data_qmain_xts$EMN_HI_D_NR_HI <- data_qmain_xts$EMN_HI /
    data_qmain_xts$NR_HI
  data_qmain_xts$EMN_NBI_D_NR_NBI <- data_qmain_xts$EMN_NBI /
    data_qmain_xts$NR_NBI
  data_qmain_xts$VEXP_HON_D_CPI_HON <- data_qmain_xts$VEXP_HON /
    data_qmain_xts$CPI_HON
  data_qmain_xts$VISCRAIR_HI_D_VIS_HI <- data_qmain_xts$VISCRAIR_HI /
    data_qmain_xts$VIS_HI
  data_qmain_xts$YLAF_R_HI_D_EAF_HI <- data_qmain_xts$YLAF_R_HI /
    data_qmain_xts$EAF_HI
  data_qmain_xts$YLMN_R_HI_D_EMN_HI <- data_qmain_xts$YLMN_R_HI /
    data_qmain_xts$EMN_HI
  data_qmain_xts$YL_ELSE_R_HI_D_E_ELSE_HI <- data_qmain_xts$YL_ELSE_R_HI /
    data_qmain_xts$E_ELSE_HI
  data_qmain_xts$YL_FIR_R_HI_D_E_FIR_HI <- data_qmain_xts$YL_FIR_R_HI /
    data_qmain_xts$E_FIR_HI
  data_qmain_xts$YL_TU_R_HI_D_E_TU_HI <- data_qmain_xts$YL_TU_R_HI /
    data_qmain_xts$E_TU_HI
  data_qmain_xts$YS_HI_D_Y_HI <- data_qmain_xts$YS_HI /
    data_qmain_xts$Y_HI
  data_qmain_xts$YTRNSF_R_HI_D_NR_HI <- data_qmain_xts$YTRNSF_R_HI /
    data_qmain_xts$NR_HI

  data_qmain_xts$OCUPP_HAW <- data_qmain_xts$OCUPPADJ_HAW
  data_qmain_xts$OCUPP_HI <- data_qmain_xts$OCUPPADJ_HI
  data_qmain_xts$OCUPP_HON <- data_qmain_xts$OCUPPADJ_HON
  data_qmain_xts$OCUPP_KAU <- data_qmain_xts$OCUPPADJ_KAU
  data_qmain_xts$OCUPP_MAU <- data_qmain_xts$OCUPPADJ_MAU
  data_qmain_xts$TRMS_HAW <- data_qmain_xts$TRMSADJ_HAW
  data_qmain_xts$TRMS_HI <- data_qmain_xts$TRMSADJ_HI
  data_qmain_xts$TRMS_HON <- data_qmain_xts$TRMSADJ_HON
  data_qmain_xts$TRMS_KAU <- data_qmain_xts$TRMSADJ_KAU
  data_qmain_xts$TRMS_MAU <- data_qmain_xts$TRMSADJ_MAU

  # DELETE START

  data_qmain_xts$SEASON_2 <- data_qmain_xts$IQ2
  data_qmain_xts$SEASON_3 <- data_qmain_xts$IQ3
  data_qmain_xts$SEASON_4 <- data_qmain_xts$IQ4

  dat_raw_dir <- "data/raw"
  dummies_aremos_xts <- readr::read_tsv(here::here(
    dat_raw_dir,
    stringr::str_glue("DUMMY1", ".TSV")
  )) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_glue("DUMMY2", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::mutate(
      time = lubridate::yq(.data$DATE),
      .before = "DATE",
      .keep = "unused"
    ) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., c("@" = "_"))) %>%
    fcutils::conv_xts()

  # store the variables used in qmod
  data_qmain_xts <- data_qmain_xts %>%
    tsbox::ts_c(dummies_aremos_xts) #%>%
  # DELETE END

  data_qmain_xts
}
