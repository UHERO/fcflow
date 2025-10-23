# *************************
# Prepare data for model estimation and simulation
# *************************

#' Prepare Model-Ready Data
#'
#' Filters the master dataset to the variables required by model, adds external
#' pseudo-exogenous series from existing forecast, and saves model inputs.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param data_main Optional preloaded master dataset.
#' @param model_equations Optional preloaded model object.
#'
#' @return Invisible list containing processed data, previous forecasts,
#'   equations, and the extension range.
#' @export
make_data_model <- function(
  cfg = load_forecast_cfg(),
  data_main = NULL,
  model_equations = NULL
) {
  update_data_main <- require_cfg(cfg, c("data_model", "update_data_main"))
  update_equations <- require_cfg(cfg, c("data_model", "update_equations"))
  # extension_start <- require_cfg(cfg, c("solve_model", "simulation_start"))
  # extension_end <- require_cfg(cfg, c("solve_model", "simulation_end"))
  equations_file <- require_cfg(cfg, c("combine_equations", "equations_file"))
  data_main_file <- require_cfg(cfg, c("data_main", "data_main_file"))
  data_model_file <- require_cfg(cfg, c("data_model", "data_model_file"))
  save_output <- require_cfg(cfg, c("data_model", "save_output"))

  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  equations_dir <- require_cfg(cfg, c("paths", "equations"))
  impexf_script <- require_cfg(cfg, c("paths", "impexf_script"))

  # # parse the extension start and end dates
  # extension_start <- lubridate::parse_date_time(
  #   extension_start,
  #   c("yq", "ymd")
  # ) %>%
  #   lubridate::as_date()
  # extension_end <- lubridate::parse_date_time(extension_end, c("yq", "ymd")) %>%
  #   lubridate::as_date()

  # ext_tsrange <- fcutils::p(extension_start, extension_end)

  if (is.null(model_equations)) {
    message("Load equations...")
    if (isTRUE(update_equations)) {
      model_equations <- combine_equations(cfg = cfg)
    } else {
      model_equations <- readRDS(
        file = here::here(
          equations_dir,
          equations_file %>% stringr::str_replace(".txt$", ".RDS")
        )
      )
    }
  }

  if (is.null(data_main)) {
    message("Load main data...")
    if (isTRUE(update_data_main)) {
      data_main_xts <- make_data_main(cfg = cfg)
    } else {
      data_main_xts <- readRDS(
        file = here::here(
          dat_prcsd_dir,
          data_main_file %>% stringr::str_replace(".csv$", ".RDS")
        )
      )
    }
  } else {
    data_main_xts <- data_main
  }

  varlist_model <- c(model_equations$vendog, model_equations$vexog) %>%
    unique()

  data_model_xts <- data_main_xts %>%
    tsbox::ts_pick(varlist_model)

  message(
    "Load existing forecast and update model data with exogenous drivers..."
  )
  script_result_env <- run_script_with_args(
    path = here::here(impexf_script),
    dat_raw_dir = dat_raw_dir,
    dat_prcsd_dir = dat_prcsd_dir,
    model_equations = model_equations,
    data_model_xts = data_model_xts,
    save_output = save_output
  )
  # retrieve the modified data from the script's environment
  data_model_xts <- script_result_env$data_model_xts

  if (isTRUE(save_output)) {
    message("Save model data...")
    saveRDS(
      data_model_xts,
      file = here::here(
        dat_prcsd_dir,
        data_model_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    )

    data_model_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          data_model_file
        )
      )
  }

  invisible(
    list(
      data_model = data_model_xts,
      model_equations = model_equations
    )
  )
}

if (identical(environment(), globalenv())) {
  make_data_model()
}

# **************************
# end ----
# **************************
