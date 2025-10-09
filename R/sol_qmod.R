# *************************
# Simulate QMOD
# *************************

#' Simulate the Quarterly Model
#'
#' Runs the BIMETS simulation for the supplied vintage and add-factors,
#' returning the full forecast trajectory alongside metadata.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param est_equations Optional BIMETS model object with data attached.
#' @param exog_range Optional ragged-edge metadata list.
#' @param add_factors Optional xts object of add factors.
#'
#' @return Invisible list containing the BIMETS simulation object, forecast,
#'   add factors, and exogenous range used.
#' @export
sol_qmod <- function(
  cfg = load_forecast_cfg(),
  est_equations = NULL,
  exog_range = NULL,
  add_factors = NULL
) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  sim_start <- require_cfg(cfg, c("sol_qmod", "sim_start"))
  sim_end <- require_cfg(cfg, c("sol_qmod", "sim_end"))
  sim_iter_limit <- require_cfg(cfg, c("sol_qmod", "sim_iter_limit"))
  sim_convergence <- require_cfg(cfg, c("sol_qmod", "sim_convergence"))
  save_outputs <- require_cfg(cfg, c("sol_qmod", "save_outputs"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  eqn_dir <- require_cfg(cfg, c("paths", "equations"))

  sim_start <- lubridate::parse_date_time(sim_start, c("yq", "ymd")) %>%
    lubridate::as_date()
  sim_end <- lubridate::parse_date_time(sim_end, c("yq", "ymd")) %>%
    lubridate::as_date()

  message("Get tsrange for simulation...")
  sim_tsrange <- c(
    lubridate::year(sim_start),
    lubridate::quarter(sim_start),
    lubridate::year(sim_end),
    lubridate::quarter(sim_end)
  )

  if (is.null(est_equations)) {
    message("Load estimated equations...")
    est_equations_qmod <- readRDS(
      file = here::here(
        eqn_dir,
        stringr::str_glue("est_equations_qmod_{curr_vint}.RDS")
      )
    )
  } else {
    est_equations_qmod <- est_equations
  }

  if (is.null(exog_range)) {
    message("Load ragged edge...")
    exog_range <- readRDS(
      file = here::here(
        eqn_dir,
        stringr::str_glue("exog_range_{curr_vint}.RDS")
      )
    )
  }

  if (is.null(add_factors)) {
    message("Load addfactors...")
    add_qmod_xts <- readRDS(
      file = here::here(dat_prcsd_dir, stringr::str_glue("add_qmod_{0}.RDS"))
    )
  } else {
    add_qmod_xts <- add_factors
  }

  message("Convert addfactors to bimets...")
  # BIMETS expects a list of individual time-series objects for constant adjustments
  add_qmod.bimets <- add_qmod_xts %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_tslist() %>%
    purrr::map(bimets::as.bimets)

  message("Solve model...")
  sim_qmod <- bimets::SIMULATE(
    est_equations_qmod,
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
  fcst_xts <- sim_qmod$simulation[sim_qmod$vendog, drop = FALSE] %>%
    fcutils::set_attr_tslist() %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_xts()

  if (isTRUE(save_outputs)) {
    message("Save forecast data...")
    # keep both the add factors and the forecast so analysts can tweak and plot
    add_qmod_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_glue("add_qmod_{curr_vint}.csv")
        )
      )

    saveRDS(
      fcst_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("fcst_{curr_vint}.RDS")
      )
    )
  }

  invisible(
    list(
      simulation = sim_qmod,
      forecast = fcst_xts,
      add_factors = add_qmod_xts,
      exog_range = exog_range
    )
  )
}

if (identical(environment(), globalenv())) {
  sol_qmod()
}

# **************************
# end ----
# **************************
