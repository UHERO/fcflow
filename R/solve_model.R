# *************************
# Simulate QMOD
# *************************

#' Simulate the Quarterly Model
#'
#' Runs the BIMETS simulation for the supplied vintage and add-factors,
#' returning the full forecast trajectory alongside metadata.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param estimated_equations Optional BIMETS model object with data attached.
#' @param exog_range Optional ragged-edge metadata list.
#' @param add0_factors Optional xts object of add factors set to 0.
#'
#' @return Invisible list containing the BIMETS simulation object, forecast,
#'   add factors, and exogenous range used.
#' @export
solve_model <- function(
  cfg = load_forecast_cfg(),
  estimated_equations = NULL,
  exog_range = NULL,
  add0_factors = NULL
) {
  estimated_equations_file <- require_cfg(
    cfg,
    c("make_model", "estimated_equations_file")
  )
  simulation_start <- require_cfg(cfg, c("solve_model", "simulation_start"))
  simulation_end <- require_cfg(cfg, c("solve_model", "simulation_end"))
  sim_iter_limit <- require_cfg(cfg, c("solve_model", "sim_iter_limit"))
  sim_convergence <- require_cfg(cfg, c("solve_model", "sim_convergence"))
  forecast_file <- require_cfg(cfg, c("solve_model", "forecast_file"))
  save_output <- require_cfg(cfg, c("solve_model", "save_output"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  equations_dir <- require_cfg(cfg, c("paths", "equations"))
  addfac_script <- require_cfg(cfg, c("paths", "addfac_script"))

  simulation_start <- lubridate::parse_date_time(
    simulation_start,
    c("yq", "ymd")
  ) %>%
    lubridate::as_date()
  simulation_end <- lubridate::parse_date_time(
    simulation_end,
    c("yq", "ymd")
  ) %>%
    lubridate::as_date()

  message("Get tsrange for simulation...")
  sim_tsrange <- c(
    lubridate::year(simulation_start),
    lubridate::quarter(simulation_start),
    lubridate::year(simulation_end),
    lubridate::quarter(simulation_end)
  )

  if (is.null(estimated_equations)) {
    message("Load estimated equations...")
    estimated_equations <- readRDS(
      file = here::here(
        equations_dir,
        estimated_equations_file %>% stringr::str_replace(".txt$", ".RDS")
      )
    )
  } else {
    estimated_equations <- estimated_equations
  }

  if (is.null(exog_range)) {
    message("Load ragged edge...")
    exog_range <- readRDS(
      file = here::here(
        equations_dir,
        stringr::str_glue("exog_range.RDS")
      )
    )
  }

  if (is.null(add0_factors)) {
    message("Load 0 addfactors...")
    addfactors0_xts <- readRDS(
      file = here::here(dat_prcsd_dir, stringr::str_glue("add0_qmod.RDS"))
    )
  } else {
    addfactors0_xts <- add0_factors
  }

  message("Modify addfactors...")
  # set addfactors
  script_result_env <- run_script_with_args(
    path = here::here(addfac_script),
    addfactors0_xts = addfactors0_xts
  )

  # retrieve the modified data from the script's environment
  addfactors_xts <- script_result_env$addfactors_xts

  message("Convert addfactors to bimets...")
  # BIMETS expects a list of individual time-series objects for constant adjustments
  add_qmod.bimets <- addfactors_xts %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_tslist() %>%
    purrr::map(bimets::as.bimets)

  message("Solve model...")
  simulated_model <- bimets::SIMULATE(
    estimated_equations,
    simType = "FORECAST",
    TSRANGE = sim_tsrange,
    ConstantAdjustment = add_qmod.bimets,
    Exogenize = exog_range,
    simConvergence = sim_convergence,
    simIterLimit = sim_iter_limit,
    quietly = FALSE
  )

  message("Extract forecast...")
  # extract just the endogenous forecast paths and convert back to xts for downstream steps
  fcst_xts <- simulated_model$simulation[
    simulated_model$vendog,
    drop = FALSE
  ] %>%
    fcutils::set_attr_tslist() %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_xts()

  if (isTRUE(save_output)) {
    message("Save forecast data...")
    # keep both the add factors and the forecast so analysts can tweak and plot
    addfactors_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          addfac_script %>%
            basename() %>%
            stringr::str_replace(".R$", ".csv")
        )
      )

    saveRDS(
      fcst_xts,
      file = here::here(
        dat_prcsd_dir,
        forecast_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    )

    fcst_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          forecast_file
        )
      )
  }

  invisible(
    list(
      simulation = simulated_model,
      forecast = fcst_xts,
      add_factors = addfactors_xts,
      exog_range = exog_range
    )
  )
}

if (identical(environment(), globalenv())) {
  solve_model()
}

# **************************
# end ----
# **************************
