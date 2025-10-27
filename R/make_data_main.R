# *************************
# Download and create quarterly data for model estimation/development
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
#' @return Invisible list with xts object containing the master quarterly dataset.
#' @export
make_data_main <- function(cfg = load_forecast_cfg(), indicators = NULL) {
  data_from_disk <- require_cfg(cfg, c("data_main", "data_from_disk"))
  udaman_export_id <- require_cfg(cfg, c("data_main", "udaman_export_id"))
  udaman_export_file <- require_cfg(cfg, c("data_main", "udaman_export_file"))
  extend_history <- require_cfg(cfg, c("data_main", "extend_history"))
  indicators_file <- require_cfg(cfg, c("data_main", "indicators_file"))
  data_main_file <- require_cfg(cfg, c("data_main", "data_main_file"))
  save_output <- require_cfg(cfg, c("data_main", "save_output"))

  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  extend_script <- require_cfg(cfg, c("paths", "extend_script"))
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
  if (is.null(indicators)) {
    ind_vars_xts <- readRDS(here::here(dat_raw_dir, indicators_file))
  } else {
    ind_vars_xts <- indicators
  }

  # ensure we have an xts object of indicators (necessary to avoid warnings in devtools::check())
  stopifnot(xts::is.xts(ind_vars_xts))

  # either read the UDAMAN export from disk or download fresh data,
  # then combine with indicators and convert to xts
  if (isTRUE(data_from_disk)) {
    message("Load main data from disk...")
    data_main_xts <- readr::read_csv(here::here(
      dat_raw_dir,
      udaman_export_file
    )) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts() %>%
      tsbox::ts_c(ind_vars_xts)
  } else {
    message("Load main data from udaman...")
    data_main_xts <- fcutils::get_series_exp(
      udaman_export_id,
      format = "xts",
      raw = TRUE,
      save_loc = here::here(
        dat_raw_dir,
        udaman_export_file
      )
    ) %>%
      tsbox::ts_c(ind_vars_xts)
  }

  # extend series that have a longer history available in 11Q4 AREMOS exports
  # this can eventually be removed
  if (isTRUE(extend_history)) {
    message("Extend history...")
    script_result_env <- run_script_with_args(
      path = here::here(extend_script),
      data_main_xts = data_main_xts,
      dat_raw_dir = dat_raw_dir
    )
    # retrieve the modified data from the script's environment
    data_main_xts <- script_result_env$data_main_xts
  }

  # restrict data to the bank sample period
  data_main_xts <- data_main_xts[smpl_bnk]

  message("Ad-hoc data adjustments...")
  # apply any final wrangling steps (e.g., create derived series, clean anomalies)
  script_result_env <- run_script_with_args(
    path = here::here(wrangl_script),
    data_main_xts = data_main_xts
  )
  # retrieve the modified data from the script's environment
  data_main_xts <- script_result_env$data_main_xts

  # ensure we are handing back an xts object
  stopifnot(xts::is.xts(data_main_xts))

  # save both an RDS (for code) and a CSV (for analysts) of the finished dataset
  if (isTRUE(save_output)) {
    message("Save main data...")
    saveRDS(
      data_main_xts,
      file = here::here(
        dat_prcsd_dir,
        data_main_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    )

    data_main_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          data_main_file
        )
      )
  }

  invisible(
    list(
      data_main = data_main_xts
    )
  )
}

if (identical(environment(), globalenv())) {
  make_data_main()
}

# **************************
# end ----
# **************************
