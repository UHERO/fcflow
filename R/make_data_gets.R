# *************************
# Prepare data for GETS model selection ----
# *************************

#' Create extended dataset for GETS model selection.
#'
#' Builds an extended quarterly dataset by chaining the current history with the
#' pseudo-forecast.
#'
#' @param cfg List-like configuration returned by [load_forecast_cfg()].
#' @param data_main Optional preloaded history (ts-boxable object). When
#'   `NULL`, the function reads the history csv from disk.
#' @param existing_forecast Optional pseudo-forecast dataset (ts-boxable object)
#'   When `NULL`, the function reads the csv from disk.
#'
#' @return Invisible list with xts object containing the extended quarterly dataset.
#' @export
make_data_gets <- function(
  cfg = load_forecast_cfg(),
  data_main = NULL,
  existing_forecast = NULL
) {
  # Pull core paths and filenames from the configuration.
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  history_file <- require_cfg(cfg, c("data_main", "data_main_file"))
  existing_forecast_file <- require_cfg(
    cfg,
    c(
      "data_gets",
      "existing_forecast_file"
    )
  )
  model_select_data_file <- require_cfg(
    cfg,
    c(
      "data_gets",
      "model_select_data_file"
    )
  )
  save_output <- require_cfg(cfg, c("data_gets", "save_output"))

  # Prefer provided datasets, otherwise load from disk.
  if (is.null(data_main)) {
    data_main_tbl <- readr::read_csv(
      file = here::here(dat_prcsd_dir, history_file),
      show_col_types = FALSE
    )
  } else {
    data_main_tbl <- data_main
  }

  if (is.null(existing_forecast)) {
    data_existing_fcst_tbl <- readr::read_csv(
      file = here::here(dat_prcsd_dir, existing_forecast_file),
      show_col_types = FALSE
    )
  } else {
    data_existing_fcst_tbl <- existing_forecast
  }

  qmain_names <- data_main_tbl %>%
    names() %>%
    stringr::str_subset("time", negate = TRUE)

  forecast_names <- data_existing_fcst_tbl %>%
    names() %>%
    stringr::str_subset("time", negate = TRUE)

  message(
    "These series are in the forecast but not in history: \n",
    stringr::str_flatten(
      setdiff(
        forecast_names,
        qmain_names
      ) %>%
        stringr::str_subset(
          "^IIS_|^SIS_|^IQ|^TREND|^CONST|^DUM|^SEASON|^QV",
          negate = TRUE
        ),
      collapse = ", "
    )
  )

  message(
    "These series are in the history but not in the extension: \n",
    stringr::str_flatten(
      setdiff(
        qmain_names,
        forecast_names
      ) %>%
        stringr::str_subset(
          "^IIS_|^SIS_|^IQ|^TREND|^CONST|^DUM|^SEASON|^QV",
          negate = TRUE
        ),
      collapse = ", "
    )
  )

  sers_to_extend <- intersect(
    qmain_names,
    forecast_names
  )

  sers_no_extend <- setdiff(
    qmain_names,
    intersect(qmain_names, forecast_names)
  )

  # Extend overlapping series by chaining history with the pseudo forecast.
  data_main_extended <- fcutils::multi_chain(
    data_main_tbl,
    data_existing_fcst_tbl,
    ids = sers_to_extend
  )

  # Convert to xts for downstream GETS modelling steps.
  data_main_extended_xts <- data_main_extended %>%
    fcutils::conv_xts()

  # Save artefacts when requested so other scripts can reuse the extended data.
  if (isTRUE(save_output)) {
    saveRDS(
      data_main_extended_xts,
      file = here::here(
        dat_prcsd_dir,
        model_select_data_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    )

    data_main_extended_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(dat_prcsd_dir, model_select_data_file)
      )
  }

  invisible(
    list(
      data_main_extended = data_main_extended_xts
    )
  )
}

if (identical(environment(), globalenv())) {
  make_data_gets()
}

# **************************
# end ----
# **************************
