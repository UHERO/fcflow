# **************************
# Build reusable indicator series for quarterly workflows
# **************************

#' Build Indicator Series
#'
#' Creates single-period, step, seasonal, trend, and constant indicator
#' variables across the full sample and stores them for reuse
#'
#' @param cfg Configuration list produced by [load_forecast_cfg()].
#'
#' @return Invisible xts object containing the indicator matrix.
#' @export
make_indicators <- function(cfg = load_forecast_cfg()) {
  bank_start_raw <- require_cfg(cfg, c("constants", "bank_start"))
  bank_end_raw <- require_cfg(cfg, c("constants", "bank_end"))
  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  indicators_file <- require_cfg(cfg, c("data_qmain", "indicators_file"))
  data_from_disk <- require_cfg(cfg, c("data_qmain", "data_from_disk"))

  # read from disk if the indicator and main data file already exists
  if (isTRUE(data_from_disk)) {
    return(readRDS(here::here(dat_raw_dir, indicators_file)))
  }

  # parse the bank start and end dates
  bank_start <- lubridate::parse_date_time(bank_start_raw, c("yq", "ymd")) %>%
    lubridate::as_date()
  bank_end <- lubridate::parse_date_time(bank_end_raw, c("yq", "ymd")) %>%
    lubridate::as_date()

  if (bank_end < bank_start) {
    stop("`constants$bank_end` must be on or after `constants$bank_start`.")
  }

  # create quarterly time index and labels
  time_index <- seq.Date(from = bank_start, to = bank_end, by = "quarter")
  time_labels <- stringr::str_c(
    lubridate::year(time_index),
    "Q",
    lubridate::quarter(time_index)
  )

  # number of observations
  n_obs <- length(time_index)

  # single-period impulse indicators
  iis_matrix <- diag(n_obs)
  colnames(iis_matrix) <- stringr::str_c("IIS_", time_labels)
  iis_indicators <- xts::xts(iis_matrix, order.by = time_index)

  # step indicators that stay on after the impulse quarter
  sis_matrix <- matrix(0, nrow = n_obs, ncol = n_obs)
  sis_matrix[row(sis_matrix) >= col(sis_matrix)] <- 1
  colnames(sis_matrix) <- stringr::str_c("SIS_", time_labels)
  sis_indicators <- xts::xts(sis_matrix, order.by = time_index)

  # seasonal dummies for each quarter
  season_indicators <- stats::model.matrix(
    ~ factor(lubridate::quarter(time_index)) - 1
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ c("IQ1", "IQ2", "IQ3", "IQ4")) %>%
    xts::xts(order.by = time_index)

  # time trend
  TREND <- tibble::tibble(TREND = seq_len(n_obs)) %>%
    xts::xts(order.by = time_index)

  # constant
  CONST <- tibble::tibble(CONST = rep.int(1, n_obs)) %>%
    xts::xts(order.by = time_index)

  # combine all indicators into one xts object
  indicator_vars <- cbind(
    iis_indicators,
    sis_indicators,
    season_indicators,
    TREND,
    CONST
  )

  # save the indicators for reuse in other scripts
  saveRDS(
    indicator_vars,
    file = here::here(dat_raw_dir, indicators_file)
  )

  invisible(indicator_vars)
}

if (identical(environment(), globalenv())) {
  make_indicators()
}

# **************************
# end ----
# **************************
