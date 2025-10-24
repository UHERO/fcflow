# *************************
# Interactive Forecast Workflow
# *************************
#
# This script runs the core forecast workflow interactively in an R session.
# It mirrors the targets pipeline steps but exposes simple booleans to run
# pieces incrementally for development and debugging.
#
# Usage:
# - Open in the project root (so here::here() resolves correctly).
# - Ensure renv has been restored (renv::restore()) if needed.
# - Edit run_steps to control which steps execute, then source this file.
#
# Note: load_forecast_cfg() reads the configuration file in config/config.yml.
# The path can be overridden by setting the MODDEV_CONFIG environment variable,
# or providing the path to the config.yml file.

# source(here::here("R", "clean.R")) # reset workspace to clean slate (do in console)
source(here::here("R", "setup.R")) # loads packages, helpers, and project setup

# Load configuration for the run. Returns a list of named configuration values.
# Provide a file path to change the repo default location of config.yml.
cfg <- load_forecast_cfg()
cfg_custom <- load_forecast_cfg(here::here("config/config_custom.yml"))
# Replace defaults with custom settings
cfg <- modifyList(cfg, cfg_custom)

# Toggle which steps to run. Set a step to FALSE to skip it and reuse prior artifacts.
# This makes it easy to iterate on a single portion without recomputing everything.
# Could/will add these settings to the config file.
run_steps <- list(
  # combine_equations = TRUE, #@fuleky why would we need this except when re-estimating?
  combine_equations = FALSE, #@bonham if we don't need it, set to false.

  make_data_main = TRUE,
  # make_data_main = FALSE,

  make_data_model = TRUE,
  # make_data_model = FALSE,

  make_model = TRUE,
  # make_model = FALSE,

  solve_model = TRUE,
  plot_forecast = TRUE
)

# Container to capture outputs (artifacts) from each step. Downstream steps read
# from this list when available so you can run partial workflows in memory.
# If not available, steps will fall back to loading prior saved artifacts from disk.
artifacts <- list()

# this step should only need to run if you re re-estimating the model ????
if (isTRUE(run_steps$combine_equations)) {
  # Assemble individual equation snippets into a combined model representation.
  # Stored at artifacts$equations and equations folder.
  message("Running combine_equations()...")
  artifacts$equations <- combine_equations(cfg = cfg)
}

if (isTRUE(run_steps$make_data_main)) {
  # Build the quarterly master dataset. Result stored at artifacts$data_qmain,
  # and in the processed data folder.
  message("Running make_data_main()...")
  artifacts$data_main <- make_data_main(cfg = cfg)
}

if (isTRUE(run_steps$make_data_model)) {
  # Prepare the model-ready data bundle. This step uses the master dataset
  # and the combined equations when available. Output in processed data folder.
  message("Running make_data_model()...")
  artifacts$data_model <- make_data_model(
    cfg = cfg,
    data_main = artifacts$data_main$data_main,
    model_equations = artifacts$equations$model_equations
  )
}

if (isTRUE(run_steps$make_model)) {
  # (Re)estimate equations and capture ragged-edge. The block chooses inputs from
  # artifacts when available; otherwise it falls back to previously computed objects
  # (allows partial runs). Output saved in equations folder.
  message("Running make_model()...")
  data_model_input <- if (!is.null(artifacts$data_model)) {
    # Prefer freshly generated data_model from this session
    artifacts$data_model$data_model
  } else {
    NULL
  }
  equations_input <- if (!is.null(artifacts$data_model)) {
    # Prefer freshly generated data_model from this session
    artifacts$data_model$model_equations
  } else {
    # Otherwise use the combined equations generated eerlier
    artifacts$equations$model_equations
  }
  artifacts$estimation <- make_model(
    cfg = cfg,
    data_model = data_model_input,
    model_equations = equations_input
  )
}

if (isTRUE(run_steps$solve_model)) {
  # Solve / simulate the full BIMETS model using estimated equations, initialized
  # add-factors, and exogenous ranges produced by estimation. Add-factors are
  # updated in this step with the add-factor file before simulation. Output
  # saved in processed data folder.
  message("Running solve_model()...")
  est_equations_input <- if (!is.null(artifacts$estimation)) {
    artifacts$estimation$estimated_equations
  } else {
    NULL
  }
  exog_range_input <- if (!is.null(artifacts$estimation)) {
    artifacts$estimation$exog_range
  } else {
    NULL
  }
  add0_factors_input <- if (!is.null(artifacts$estimation)) {
    artifacts$estimation$add0_factors
  } else {
    NULL
  }
  artifacts$forecast <- solve_model(
    cfg = cfg,
    estimated_equations = est_equations_input,
    exog_range = exog_range_input,
    add0_factors = add0_factors_input
  )
}

if (isTRUE(run_steps$plot_forecast)) {
  # Render plots.
  # The plotting function can load current/previous/history vintages from disk
  # unless we explicitly pass objects computed in this session — in that case
  # set cfg flags to prevent redundant loading. Output saved in output folder.
  message("Running plot_forecast()...")
  plot_cfg <- cfg$plot_forecast
  fcst_override <- if (!is.null(artifacts$forecast)) {
    artifacts$forecast$forecast
  } else {
    NULL
  }
  comp_override <- if (!is.null(artifacts$plots)) {
    artifacts$plots$comparison_forecast
  } else {
    NULL
  }
  hist_override <- if (!is.null(artifacts$plots)) {
    artifacts$plots$history
  } else {
    NULL
  }

  # This modifies config and may lead to unexpected behavior. If so, comment out.
  # If we provide in-memory objects, instruct plot_forecast not to re-load them.
  if (!is.null(fcst_override)) {
    plot_cfg$load_curr_vint <- FALSE
  }
  if (!is.null(comp_override)) {
    plot_cfg$load_prev_vint <- FALSE
  }
  if (!is.null(hist_override)) {
    plot_cfg$load_history <- FALSE
  }

  cfg$plot_forecast <- plot_cfg
  artifacts$plots <- plot_forecast(
    cfg = cfg,
    forecast = fcst_override,
    comparison_forecast = comp_override,
    history = hist_override
  )
}

# Return the collected artifacts invisibly so a user can assign them when
# sourcing this script (e.g., a <- source("R/forecast_work.R")).
invisible(artifacts)

# **************************
# end ----
# **************************
