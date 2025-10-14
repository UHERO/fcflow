# **************************
# Data helpers for forecast
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
