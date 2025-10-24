# **************************
# Wrangle data
# **************************

#' Enrich Master Quarterly Dataset
#'
#' Applies indicator adjustments, fills missing histories, and adds
#' constructed series required for the quarterly model.
#'
#' @param data_main_xts xts object produced by [make_data_main()].
#'
#' @return Modified xts object with additional constructed series.
#' @keywords internal
wrangle_data <- function(data_main_xts) {
  stopifnot(!missing(data_main_xts))

  # outline: create helper lists of mnemonic-location combinations, backfill pandemic holes,
  # compute price ratios/shares, and ensure select series have smooth historical coverage
  # TODO: the addfactor file should not be doing any history wrangling. move that here.

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

  data_main_xts[
    fcutils::p(
      fcutils::find_start(data_main_xts$VADC_HI),
      fcutils::find_start(data_main_xts$VADCCRAIR_HI)
    ),
    vadccrair_list
  ] <- 0
  data_main_xts[
    fcutils::p(
      fcutils::find_start(data_main_xts$VLOS_HI),
      fcutils::find_start(data_main_xts$VLOSCRAIR_HI)
    ),
    vloscrair_list
  ] <- 0
  data_main_xts[
    fcutils::p(
      fcutils::find_start(data_main_xts$VIS_HI),
      fcutils::find_start(data_main_xts$VISCRAIR_HI)
    ),
    viscrair_list
  ] <- 0

  # zero out pandemic-era observations and single-point outliers so downstream averages behave
  data_main_xts[fcutils::pq(2020.2, 2022.1), vadccrair_list] <- 0
  data_main_xts[fcutils::pq(2020.2, 2022.1), vloscrair_list] <- 0
  data_main_xts[fcutils::pq(2020.2, 2022.1), viscrair_list] <- 0
  data_main_xts$VLOSJP_KAU[fcutils::pq(2020.2, 2021.3)] <- 0
  data_main_xts$VLOSJP_MAU[fcutils::pq(2020.2, 2021.1)] <- 0
  data_main_xts$VLOSJP_HAW[fcutils::pq(2020.2, 2021.1)] <- 0
  data_main_xts$VLOSJP_HON[fcutils::pq(2020.2, 2020.2)] <- 0
  data_main_xts$VLOSJP_HI[fcutils::pq(2020.2, 2020.2)] <- 0
  data_main_xts$VLOSRES_MAU[fcutils::pq(2001.4, 2001.4)] <- 0

  data_main_xts$VEXP_HI <- data_main_xts$VEXP_HI %>%
    fcutils::interpol(data_main_xts$VADC_HI)
  data_main_xts$VEXP_R_HI <- data_main_xts$VEXP_R_HI %>%
    fcutils::interpol(data_main_xts$VADC_HI)
  data_main_xts$VEXP_RB_HI <- data_main_xts$VEXP_RB_HI %>%
    fcutils::interpol(data_main_xts$VADC_HI)
  data_main_xts$VEXP_HON <- data_main_xts$VEXP_HON %>%
    fcutils::interpol(data_main_xts$VADC_HON)
  data_main_xts$YPJ_SV_R_HI <- data_main_xts$YPJ_SV_R_HI %>%
    fcutils::interpol(data_main_xts$YPJ_R_HI)

  # convert to wide form so we can compute price ratios and geographic shares
  data_main_xts <- data_main_xts %>%
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

  data_main_xts[smpl_aginc, aglist] <- zoo::na.approx(data_main_xts[
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

  data_main_xts[smpl_ylinc, yllist] <- zoo::na.approx(data_main_xts[
    smpl_ylinc,
    yllist
  ])

  data_main_xts$YLAG_R_NBI <- data_main_xts$YLAG_R_HI -
    data_main_xts$YLAG_R_HON
  data_main_xts$YPJAG_R_NBI <- data_main_xts$YLAG_R_NBI /
    data_main_xts$EAG_NBI
  data_main_xts$YLAG_NBI <- data_main_xts$YLAG_HI -
    data_main_xts$YLAG_HON

  data_main_xts$YL_SV_HI <- data_main_xts$YLHC_HI +
    data_main_xts$YLAF_HI +
    data_main_xts$YL_ELSE_HI
  data_main_xts$YL_SV_R_HI <- data_main_xts$YLHC_R_HI +
    data_main_xts$YLAF_R_HI +
    data_main_xts$YL_ELSE_R_HI

  data_main_xts$GDP_R_RES <- data_main_xts$GDP_R_US +
    data_main_xts$GDP_R_JP / data_main_xts$YXR_JP

  data_main_xts$LFPR_HI <- data_main_xts$LF_HI / data_main_xts$NR_HI
  data_main_xts$LFPR_HON <- data_main_xts$LF_HON / data_main_xts$NR_HON
  data_main_xts$LFPR_NBI <- data_main_xts$LF_NBI / data_main_xts$NR_NBI

  data_main_xts$EMN_HI_D_NR_HI <- data_main_xts$EMN_HI /
    data_main_xts$NR_HI
  data_main_xts$EMN_NBI_D_NR_NBI <- data_main_xts$EMN_NBI /
    data_main_xts$NR_NBI
  data_main_xts$VEXP_HON_D_CPI_HON <- data_main_xts$VEXP_HON /
    data_main_xts$CPI_HON
  data_main_xts$VISCRAIR_HI_D_VIS_HI <- data_main_xts$VISCRAIR_HI /
    data_main_xts$VIS_HI
  data_main_xts$YLAF_R_HI_D_EAF_HI <- data_main_xts$YLAF_R_HI /
    data_main_xts$EAF_HI
  data_main_xts$YLMN_R_HI_D_EMN_HI <- data_main_xts$YLMN_R_HI /
    data_main_xts$EMN_HI
  data_main_xts$YL_ELSE_R_HI_D_E_ELSE_HI <- data_main_xts$YL_ELSE_R_HI /
    data_main_xts$E_ELSE_HI
  data_main_xts$YL_FIR_R_HI_D_E_FIR_HI <- data_main_xts$YL_FIR_R_HI /
    data_main_xts$E_FIR_HI
  data_main_xts$YL_TU_R_HI_D_E_TU_HI <- data_main_xts$YL_TU_R_HI /
    data_main_xts$E_TU_HI
  data_main_xts$YS_HI_D_Y_HI <- data_main_xts$YS_HI /
    data_main_xts$Y_HI
  data_main_xts$YTRNSF_R_HI_D_NR_HI <- data_main_xts$YTRNSF_R_HI /
    data_main_xts$NR_HI

  data_main_xts$OCUPP_HAW <- data_main_xts$OCUPPADJ_HAW
  data_main_xts$OCUPP_HI <- data_main_xts$OCUPPADJ_HI
  data_main_xts$OCUPP_HON <- data_main_xts$OCUPPADJ_HON
  data_main_xts$OCUPP_KAU <- data_main_xts$OCUPPADJ_KAU
  data_main_xts$OCUPP_MAU <- data_main_xts$OCUPPADJ_MAU
  data_main_xts$TRMS_HAW <- data_main_xts$TRMSADJ_HAW
  data_main_xts$TRMS_HI <- data_main_xts$TRMSADJ_HI
  data_main_xts$TRMS_HON <- data_main_xts$TRMSADJ_HON
  data_main_xts$TRMS_KAU <- data_main_xts$TRMSADJ_KAU
  data_main_xts$TRMS_MAU <- data_main_xts$TRMSADJ_MAU

  # DELETE START

  data_main_xts$SEASON_2 <- data_main_xts$IQ2
  data_main_xts$SEASON_3 <- data_main_xts$IQ3
  data_main_xts$SEASON_4 <- data_main_xts$IQ4

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

  # store the variables used in the model
  data_main_xts <- data_main_xts %>%
    tsbox::ts_c(dummies_aremos_xts) #%>%
  # DELETE END

  data_main_xts
}

data_main_xts <- wrangle_data(data_main_xts)
