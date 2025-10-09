# *************************
# Estimate QMOD
# *************************

#' Estimate or Refresh QMOD
#'
#' Loads data and equations, optionally re-estimates the quarterly model,
#' and persists the fitted model, add factors, and ragged-edge metadata.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param data_qmod Optional preloaded xts dataset (defaults to saved output).
#' @param equations_qmod Optional preloaded model definition.
#'
#' @return Invisible list with estimation results, add factors, exogenous
#'   ranges, and the underlying data.
#' @export
make_qmod <- function(
  cfg = load_forecast_cfg(),
  data_qmod = NULL,
  equations_qmod = NULL
) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  reestimate <- require_cfg(cfg, c("make_qmod", "reestimate"))
  save_eq <- require_cfg(cfg, c("make_qmod", "save_eq"))
  max_lag <- require_cfg(cfg, c("make_qmod", "max_lag"))
  force_est_tsrange <- require_cfg(cfg, c("make_qmod", "force_tsrange"))
  est_start <- require_cfg(cfg, c("make_qmod", "est_start"))
  est_end <- require_cfg(cfg, c("make_qmod", "est_end"))
  save_outputs <- require_cfg(cfg, c("make_qmod", "save_outputs"))

  # outline: prepare directories, load data/equations, optionally re-estimate every equation,
  # and then cache the add factors plus ragged-edge metadata for sims and plotting
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  eqn_dir <- require_cfg(cfg, c("paths", "equations"))

  est_start <- lubridate::parse_date_time(est_start, c("yq", "ymd")) %>%
    lubridate::as_date()
  est_end <- lubridate::parse_date_time(est_end, c("yq", "ymd")) %>%
    lubridate::as_date()

  message("Get global tsrange for estimation...")
  est_tsrange <- c(
    lubridate::year(est_start),
    lubridate::quarter(est_start),
    lubridate::year(est_end),
    lubridate::quarter(est_end)
  )

  if (is.null(equations_qmod)) {
    message("Load equations...")
    # fall back to the saved equations bundle if none was provided by the caller
    equations_qmod <- readRDS(
      file = here::here(
        eqn_dir,
        stringr::str_glue("equations_qmod_{curr_vint}.RDS")
      )
    )
  }

  if (is.null(data_qmod)) {
    message("Load data for qmod...")
    # load the model-ready data set that `make_data_qmod()` saved earlier
    data_qmod_xts <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_qmod_{curr_vint}.RDS")
      )
    )
  } else {
    data_qmod_xts <- data_qmod
  }

  message("Initialize addfactors...")
  # add factors start at zero so analysts can later hand-edit the CSV or R file
  add_qmod_xts <- data_qmod_xts %>%
    tsbox::ts_pick(equations_qmod$vendog) %>%
    tsbox::ts_tbl() %>%
    dplyr::mutate(value = 0) %>%
    tsbox::ts_xts()

  message("Capture ragged edge...")
  # capture the ragged edge (last historic observation) for each endogenous variable
  exog_range <- data_qmod_xts %>%
    tsbox::ts_pick(equations_qmod$vendog) %>%
    tsbox::ts_tslist() %>%
    purrr::map(tsbox::ts_summary) %>%
    purrr::map(
      function(x) {
        c(
          lubridate::year(x$start),
          lubridate::quarter(x$start),
          lubridate::year(x$end),
          lubridate::quarter(x$end)
        )
      }
    )

  message("Convert from xts to bimets...")
  data_qbimets <- data_qmod_xts %>%
    tsbox::ts_tbl() %>%
    tidyr::drop_na() %>%
    tsbox::ts_tslist() %>%
    purrr::map(bimets::as.bimets)

  if (isTRUE(reestimate)) {
    message("Set local tsrange for estimation...")
    # load the data into each equation and re-estimate so every coefficient reflects the new vintage
    equations_data_qmod <- bimets::LOAD_MODEL_DATA(
      model = equations_qmod,
      modelData = data_qbimets
    ) %>%
      fcutils::set_tsrange(max_lag = max_lag)

    if (isTRUE(save_eq)) {
      message("Sink output...")
      est_output_file <- here::here(
        eqn_dir,
        stringr::str_glue("est_equations_qmod_{curr_vint}.txt")
      )
      sink(est_output_file)
      on.exit(sink(), add = TRUE)
      cat("\n", "ESTIMATION RESULTS", "\n", sep = "")
    }

    message("Estimate qmod equations...")
    est_equations_qmod <- bimets::ESTIMATE(
      equations_data_qmod,
      eqList = equations_data_qmod$vendogBehaviorals,
      TSRANGE = est_tsrange,
      forceTSRANGE = force_est_tsrange,
      quietly = FALSE
    )

    if (isTRUE(save_eq)) {
      message("Add identities to sink...")
      cat("\n", "ESTIMATION RESULTS - IDENTITIES", "\n", sep = "")
      for (i in seq_along(est_equations_qmod$identities)) {
        cat("\n", "Identity ", i, "\n", sep = "")
        cat(est_equations_qmod$identities[[i]]$eqFull, "\n")
      }
    }
  } else {
    message("Load previously estimated equations...")
    est_equations_qmod <- readRDS(
      file = here::here(
        eqn_dir,
        stringr::str_glue("est_equations_qmod_{curr_vint}.RDS")
      )
    )

    message("Add data to previously estimated equations...")
    est_equations_qmod <- bimets::LOAD_MODEL_DATA(
      model = est_equations_qmod,
      modelData = data_qbimets
    )
  }

  if (isTRUE(save_outputs)) {
    message("Save model data...")
    # persist add factors, ragged-edge metadata, and the (possibly re-estimated) BIMETS object
    saveRDS(
      add_qmod_xts,
      file = here::here(dat_prcsd_dir, stringr::str_glue("add_qmod_{0}.RDS"))
    )

    saveRDS(
      exog_range,
      file = here::here(
        eqn_dir,
        stringr::str_glue("exog_range_{curr_vint}.RDS")
      )
    )

    saveRDS(
      est_equations_qmod,
      file = here::here(
        eqn_dir,
        stringr::str_glue("est_equations_qmod_{curr_vint}.RDS")
      )
    )
  }

  invisible(
    list(
      est_equations = est_equations_qmod,
      add_factors = add_qmod_xts,
      exog_range = exog_range,
      data_qmod = data_qmod_xts
    )
  )
}

if (identical(environment(), globalenv())) {
  make_qmod()
}

# **************************
# end ----
# **************************
