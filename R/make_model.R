# *************************
# Estimate QMOD
# *************************

#' Estimate or Refresh Model
#'
#' Loads data and equations, optionally re-estimates the quarterly model,
#' and persists the fitted model, add factors, and ragged-edge metadata.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param data_model Optional preloaded xts dataset (defaults to saved output).
#' @param model_equations Optional preloaded model definition.
#'
#' @return Invisible list with estimation results, add factors, exogenous
#'   ranges, and the underlying data.
#' @export
make_model <- function(
  cfg = load_forecast_cfg(),
  data_model = NULL,
  model_equations = NULL
) {
  # outline: prepare directories, load data/equations, optionally re-estimate every equation,
  # and then cache the add factors plus ragged-edge metadata for sims and plotting
  equations_file <- require_cfg(cfg, c("combine_equations", "equations_file"))
  reestimate <- require_cfg(cfg, c("make_model", "reestimate"))
  save_eq <- require_cfg(cfg, c("make_model", "save_eq"))
  discover_tsrange <- require_cfg(cfg, c("make_model", "discover_tsrange"))
  max_lag <- require_cfg(cfg, c("make_model", "max_lag"))
  force_est_tsrange <- require_cfg(cfg, c("make_model", "force_tsrange"))
  estimation_start <- require_cfg(cfg, c("make_model", "estimation_start"))
  estimation_end <- require_cfg(cfg, c("make_model", "estimation_end"))
  data_model_file <- require_cfg(cfg, c("data_model", "data_model_file"))
  estimated_equations_file <- require_cfg(
    cfg,
    c("make_model", "estimated_equations_file")
  )
  save_output <- require_cfg(cfg, c("make_model", "save_output"))

  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  equations_dir <- require_cfg(cfg, c("paths", "equations"))

  estimation_start <- lubridate::parse_date_time(
    estimation_start,
    c("yq", "ymd")
  ) %>%
    lubridate::as_date()
  estimation_end <- lubridate::parse_date_time(
    estimation_end,
    c("yq", "ymd")
  ) %>%
    lubridate::as_date()

  message("Get global tsrange for estimation...")
  est_tsrange <- c(
    lubridate::year(estimation_start),
    lubridate::quarter(estimation_start),
    lubridate::year(estimation_end),
    lubridate::quarter(estimation_end)
  )

  if (is.null(model_equations)) {
    message("Load equations...")
    # fall back to the saved equations bundle if none was provided by the caller
    model_equations <- readRDS(
      file = here::here(
        equations_dir,
        equations_file %>% stringr::str_replace(".txt$", ".RDS")
      )
    )
  }

  if (is.null(data_model)) {
    message("Load data for qmod...")
    # load the model-ready data set that `data_model()` saved earlier
    data_model_xts <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        data_model_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    )
  } else {
    data_model_xts <- data_model
  }

  message("Initialize addfactors...")
  # add factors start at zero so analysts can later hand-edit the CSV or R file
  addfactors0_xts <- data_model_xts %>%
    tsbox::ts_pick(model_equations$vendog) %>%
    tsbox::ts_tbl() %>%
    dplyr::mutate(value = 0) %>%
    tsbox::ts_xts()

  message("Capture ragged edge...")
  # capture the ragged edge (last historic observation) for each endogenous variable
  exog_range <- data_model_xts %>%
    tsbox::ts_pick(model_equations$vendog) %>%
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
  data_qbimets <- data_model_xts %>%
    tsbox::ts_tbl() %>%
    tidyr::drop_na() %>%
    tsbox::ts_tslist() %>%
    purrr::map(bimets::as.bimets)

  if (isTRUE(reestimate)) {
    message("Load data into model...")
    # load the data into each equation and re-estimate so every coefficient reflects the new vintage
    model_equations_with_data <- bimets::LOAD_MODEL_DATA(
      model = model_equations,
      modelData = data_qbimets
    )

    if (isTRUE(discover_tsrange)) {
      message("Set local tsrange for estimation...")
      model_equations_with_data <- model_equations_with_data %>%
        fcutils::set_tsrange(max_lag = max_lag)
    }

    if (isTRUE(save_eq)) {
      message("Sink output in ", equations_dir)
      sink(here::here(
        equations_dir,
        estimated_equations_file
      ))
      on.exit(sink(), add = TRUE)
      cat("\n", "ESTIMATION RESULTS", "\n", sep = "")
    }

    message("Estimate qmod equations...")
    estimated_equations <- bimets::ESTIMATE(
      model_equations_with_data,
      eqList = model_equations_with_data$vendogBehaviorals,
      TSRANGE = est_tsrange,
      forceTSRANGE = force_est_tsrange,
      quietly = FALSE
    )

    if (isTRUE(save_eq)) {
      message("Add identities to sink...")
      cat("\n", "ESTIMATION RESULTS - IDENTITIES", "\n", sep = "")
      for (i in seq_along(estimated_equations$identities)) {
        cat("\n", "Identity ", i, "\n", sep = "")
        cat(estimated_equations$identities[[i]]$eqFull, "\n")
      }
    }
  } else {
    message("Load previously estimated equations...")
    estimated_equations <- readRDS(
      file = here::here(
        equations_dir,
        estimated_equations_file %>% stringr::str_replace(".txt$", ".RDS")
      )
    )

    message("Add data to previously estimated equations...")
    estimated_equations <- bimets::LOAD_MODEL_DATA(
      model = estimated_equations,
      modelData = data_qbimets
    )
  }

  if (isTRUE(save_output)) {
    message("Save model data...")
    # save add factors, ragged-edge metadata, and the (possibly re-estimated) BIMETS object
    saveRDS(
      addfactors0_xts,
      file = here::here(dat_prcsd_dir, stringr::str_glue("add0_qmod.RDS"))
    )

    saveRDS(
      exog_range,
      file = here::here(
        equations_dir,
        stringr::str_glue("exog_range.RDS")
      )
    )

    saveRDS(
      estimated_equations,
      file = here::here(
        equations_dir,
        estimated_equations_file %>% stringr::str_replace(".txt$", ".RDS")
      )
    )
  }

  invisible(
    list(
      estimated_equations = estimated_equations,
      add0_factors = addfactors0_xts,
      exog_range = exog_range,
      data_model = data_model_xts
    )
  )
}

if (identical(environment(), globalenv())) {
  make_model()
}

# **************************
# end ----
# **************************
