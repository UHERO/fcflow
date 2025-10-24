# *************************
# Interactive Model Selection Workflow
# *************************
#
# This script runs the model selectoin interactively in an R session.
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
# cfg <- load_forecast_cfg(here::here("config", "mod_sel_config.yml"))
cfg <- load_forecast_cfg()
cfg_custom <- load_forecast_cfg(here::here("config/config_custom.yml"))
cfg_modselect <- load_forecast_cfg(here::here("config/config_modselect.yml"))
# Replace defaults with custom settings
cfg <- modifyList(cfg, cfg_custom)
cfg <- modifyList(cfg, cfg_modselect)

# Toggle which steps to run. Set a step to FALSE to skip it and reuse prior artifacts.
# This makes it easy to iterate on a single portion without recomputing everything.
run_steps <- list(
  make_data_gets = TRUE, # can set to FALSE once data set is prepared
  gets_model_select = TRUE
)

# Container to capture outputs (artifacts) from each step. Downstream steps read
# from this list when available so you can run partial workflows in memory.
# If not available, steps will fall back to loading prior saved artifacts from disk.
artifacts <- list()

if (isTRUE(run_steps$make_data_gets)) {
  # Build the quarterly master dataset. Result stored at artifacts$data_gets,
  # and in the processed data folder.
  message("Running make_data_gets()...")
  artifacts$data_gets <- make_data_gets(cfg = cfg)
}

if (isTRUE(run_steps$gets_model_select)) {
  # Generate individual equation snippet.
  # Stored at artifacts$model_select and equations folder.
  message("Running gets_model_select()...")
  artifacts$model_select <- select_model(
    cfg = cfg,
    model_select_data = artifacts$data_gets$data_main_extended,
    indicators = NULL
  )
}

# Return the collected artifacts invisibly so a user can assign them when
# sourcing this script (e.g., a <- source("R/model_selection.R")).
invisible(artifacts)

# **************************
# end ----
# **************************

# HOW TO UPDATE AN ESTIMATED MODEL WITH NEW/MODIFIED EQUATIONS
# The process is actually the reverse: HOW TO BRING PREVIOUSLY ESTIMATED EQUATIONS INTO A NEW MODEL
# Start by making a backup copy of the original model's equations (txt file) and estimated equations (RDS file). This is model_old.
# Now modify the working set of equations. This is model_new. Re-specify some equations, add new equations, and remove not needed equations.
# Do this by modifying the list of equations in a _eq.txt file (or replace an existing equation) with the equation identified above.
# Run the combine_equations() function to assemble the new model equations file. In forecast_work this is artifacts$equations$model_equations.
# Run the make_model() function to estimate the modified/new model equations (ok to estimate all). This is artifacts$estimation$estimated_equations.
# Use the fcutils::update_eqs() function to replace in model_new those equations that should remain the same as in model_old.
# The fcutils::update_eqs() function copies the estimated equations from model_old into model_new.
# Equations that are to remain unchanged have to be present in both model_old and model_new, and not present in eqList.
# The argument eqList contains the names of updated/new behavioral equations (equations missing from model_new are removed).
