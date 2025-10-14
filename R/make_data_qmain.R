# *************************
# Download and create quarterly data for qmod estimation/development
# *************************

#' Build Master Quarterly Dataset
#'
#' Downloads (or reloads) history, forecast inputs, adds indicators,
#' and saves the quarterly master data file for the current vintage.
#'
#' @param cfg List-like configuration, normally from [load_forecast_cfg()].
#' @param indicators Optional xts object of indicator variables; when `NULL`
#'   the function reloads from disk using the configured file name.
#'
#' @return Invisible xts object containing the master quarterly dataset.
#' @export
make_data_qmain <- function(cfg = load_forecast_cfg(), indicators = NULL) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  data_from_disk <- require_cfg(cfg, c("data_qmain", "data_from_disk"))
  exp_id_q <- require_cfg(cfg, c("data_qmain", "exp_id_q"))
  extend_history <- require_cfg(cfg, c("data_qmain", "extend_history"))
  indicators_file <- require_cfg(cfg, c("data_qmain", "indicators_file"))
  save_outputs <- require_cfg(cfg, c("data_qmain", "save_outputs"))

  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  wrangl_script <- require_cfg(cfg, c("paths", "wrangl_script"))

  bank_start <- require_cfg(cfg, c("constants", "bank_start"))
  bank_end <- require_cfg(cfg, c("constants", "bank_end"))

  # parse the bank start and end dates
  bank_start <- lubridate::parse_date_time(bank_start, c("yq", "ymd")) %>%
    lubridate::as_date()
  bank_end <- lubridate::parse_date_time(bank_end, c("yq", "ymd")) %>%
    lubridate::as_date()

  # create a sample period object
  smpl_bnk <- fcutils::p(bank_start, bank_end)

  # historical indicator series (calendar dummies, etc.) are stored once and reused
  message("Load indicators...")
  if (is.null(indicators_file)) {
    ind_vars_xts <- make_indicators(cfg)
  } else {
    ind_vars_xts <- readRDS(here::here(dat_raw_dir, indicators_file))
  }

  # ensure we have an xts object of indicators (necessary to avoid warnings in devtools::check())
  stopifnot(xts::is.xts(ind_vars_xts))

  # either read the UDAMAN export from disk or download fresh data,
  # then combine with indicators and convert to xts
  if (isTRUE(data_from_disk)) {
    message("Load main data from disk...")
    data_qmain_xts <- readr::read_csv(here::here(
      dat_raw_dir,
      stringr::str_glue("{exp_id_q}.csv")
    )) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts() %>%
      tsbox::ts_c(ind_vars_xts)
  } else {
    message("Load main data from udaman...")
    data_qmain_xts <- fcutils::get_series_exp(
      exp_id_q,
      format = "xts",
      raw = TRUE,
      save_loc = here::here(dat_raw_dir, stringr::str_glue("{exp_id_q}.csv"))
    ) %>%
      tsbox::ts_c(ind_vars_xts)
  }

  # extend series that have a longer history available in 11Q4 AREMOS exports
  # this can eventually be removed
  if (isTRUE(extend_history)) {
    message("Extend history...")
    data_qmain_xts <- extend_qmain_history(
      data_qmain_xts = data_qmain_xts,
      dat_raw_dir = dat_raw_dir
    )
  }

  message("Ad-hoc data adjustments...")
  # apply any final wrangling steps (e.g., create derived series, clean anomalies)
  script_result_env <- run_script_with_args(
    path = here::here(wrangl_script),
    data_qmain_xts = data_qmain_xts
  )

  # retrieve the modified data from the script's environment
  data_qmain_xts <- script_result_env$data_qmain_xts

  # ensure we are handing back an xts object
  stopifnot(xts::is.xts(data_qmain_xts))

  # save both an RDS (for code) and a CSV (for analysts) of the finished dataset
  if (isTRUE(save_outputs)) {
    message("Save main data...")
    saveRDS(
      data_qmain_xts[smpl_bnk],
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_qmain_{curr_vint}.RDS")
      )
    )

    data_qmain_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_glue("data_qmain_{curr_vint}.csv")
        )
      )
  }

  invisible(data_qmain_xts[smpl_bnk])
}

if (identical(environment(), globalenv())) {
  make_data_qmain()
}

# **************************
# end ----
# **************************
