# *************************
# Prepare data for GETS model selection ----
# *************************

#' Create extended dataset for GETS model selection.
#'
#' Builds an extended quarterly dataset by chaining the current history with the
#' pseudo-forecast.
#'
#' @param cfg List-like configuration returned by [load_forecast_cfg()].
#' @param data_qmain Optional preloaded history (ts-boxable object). When
#'   `NULL`, the function reads the history csv from disk.
#' @param existing_forecast Optional pseudo-forecast dataset (ts-boxable object)
#'   When `NULL`, the function reads the csv from disk.
#'
#' @return Invisibly returns the extended xts.
#' @export
make_data_gets <- function(
  cfg = load_forecast_cfg(),
  data_qmain = NULL,
  existing_forecast = NULL
) {
  # Pull core paths and filenames from the configuration.
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  history_file <- require_cfg(cfg, c("make_data_gets", "history_file"))
  existing_fcst_file <- require_cfg(
    cfg,
    c(
      "make_data_gets",
      "existing_fcst_file"
    )
  )
  mod_select_data_file <- require_cfg(
    cfg,
    c(
      "make_data_gets",
      "mod_select_data_file"
    )
  )
  save_output <- require_cfg(cfg, c("make_data_gets", "save_output"))

  # Prefer provided datasets, otherwise load from disk.
  if (is.null(data_qmain)) {
    data_qmain_tbl <- readr::read_csv(
      file = here::here(dat_prcsd_dir, history_file),
      show_col_types = FALSE
    )
  } else {
    data_qmain_tbl <- data_qmain
  }

  if (is.null(existing_forecast)) {
    data_existing_fcst_tbl <- readr::read_csv(
      file = here::here(dat_prcsd_dir, existing_fcst_file),
      show_col_types = FALSE
    )
  } else {
    data_existing_fcst_tbl <- existing_forecast
  }

  qmain_names <- data_qmain_tbl %>%
    names() %>%
    stringr::str_subset("time", negate = TRUE)

  forecast_names <- data_existing_fcst_tbl %>%
    names() %>%
    stringr::str_subset("time", negate = TRUE)

  message(
    "These series are in the forecast but not in history: ",
    stringr::str_flatten(
      setdiff(
        forecast_names,
        qmain_names
      ),
      collapse = ", "
    )
  )

  message(
    "These series are in the history but not in the extension: ",
    stringr::str_flatten(
      setdiff(
        qmain_names,
        forecast_names
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
  data_qmain_ext <- fcutils::multi_chain(
    data_qmain_tbl,
    data_existing_fcst_tbl,
    ids = sers_to_extend
  )

  # data_qmain_ext_augm <- data_qmain_ext %>%
  #   dplyr::mutate(TRMSADJ_HON_HI = TRMSADJ_HON / TRMSADJ_HI) %>%
  #   dplyr::mutate(VADC_PRM_HI = VADC_HI * PRM_HI) %>%
  #   dplyr::mutate(PPRM_TRMSADJ_HI = PPRM_HI * TRMSADJ_HI) %>%
  #   dplyr::mutate(PPRM_TRMSADJ_HAW = PPRM_HAW * TRMSADJ_HAW) %>%
  #   dplyr::mutate(PPRM_TRMSADJ_HON = PPRM_HON * TRMSADJ_HON) %>%
  #   dplyr::mutate(PPRM_TRMSADJ_KAU = PPRM_KAU * TRMSADJ_KAU) %>%
  #   dplyr::mutate(PPRM_TRMSADJ_MAU = PPRM_MAU * TRMSADJ_MAU) %>%
  #   dplyr::rename("CPI_B_HON_SHORT" = "CPI_B_HON") %>%
  #   dplyr::left_join(
  #     tsbox::ts_chain(
  #       data_qmain_ext %>%
  #         tsbox::ts_long() %>%
  #         tsbox::ts_pick("CPI_B_HON") %>%
  #         tsbox::ts_na_omit(),
  #       data_qmain_ext %>%
  #         tsbox::ts_long() %>%
  #         tsbox::ts_pick("CPI_HON") %>%
  #         tsbox::ts_na_omit()
  #     ) %>%
  #       dplyr::mutate(id = "CPI_B_HON") %>%
  #       tsbox::ts_wide(),
  #     by = "time"
  #   )

  # Convert to xts for downstream GETS modelling steps.
  data_qmain_ext_xts <- data_qmain_ext %>%
    fcutils::conv_xts()

  # Save artefacts when requested so other scripts can reuse the extended data.
  if (isTRUE(save_output)) {
    saveRDS(
      data_qmain_ext_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_qmain_ext.RDS")
      )
    )

    data_qmain_ext_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(dat_prcsd_dir, "data_qmain_ext.csv")
      )
  }

  invisible(data_qmain_ext_xts)
}

if (identical(environment(), globalenv())) {
  make_data_gets()
}

# **************************
# end ----
# **************************
