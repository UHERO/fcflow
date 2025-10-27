# **************************
# Forecast configuration helpers
# **************************

#' Load Forecast Configuration
#'
#' Reads the YAML configuration file that stores forecast parameters
#' such as vintages, output settings, and workflow toggles.
#'
#' @param path Optional explicit path to the YAML configuration. When `NULL`,
#'   the loader checks `MODDEV_CONFIG`, the packaged default under
#'   `inst/extdata/config/current_config.yml`, and finally `config/current_config.yml` in the
#'   project root.
#'
#' @return A named list containing configuration entries.
#' @export
load_forecast_cfg <- function(path = NULL) {
  candidate_paths <- c(
    path,
    Sys.getenv("MODDEV_CONFIG", ""),
    here::here("config", "config.yml"),
    system.file("extdata", "config", "config.yml", package = "fcflow")
  )

  candidate_paths <- Filter(
    function(x) !is.null(x) && nzchar(x),
    unique(candidate_paths)
  )

  resolved <- candidate_paths[file.exists(candidate_paths)][1]

  if (is.na(resolved) || !nzchar(resolved)) {
    stop(
      "Configuration file not found. Checked: ",
      paste(candidate_paths, collapse = ", ")
    )
  }

  cfg <- yaml::read_yaml(resolved, eval.expr = FALSE)
  attr(cfg, "config_path") <- resolved
  cfg
}

#' Require Configuration Value
#'
#' Traverses a configuration list using a sequence of names, returning the
#' located value or stopping with a descriptive error if any level is missing.
#'
#' @param cfg Root configuration list.
#' @param path Character vector of names representing the path within `cfg`.
#'
#' @return The configuration value located at `path`.
#' @keywords internal
#' @noRd
require_cfg <- function(cfg, path) {
  value <- cfg
  for (name in path) {
    if (!is.list(value) || is.null(value[[name]])) {
      stop(sprintf(
        "Configuration value `%s` is not set.",
        paste(path, collapse = "$")
      ))
    }
    value <- value[[name]]
  }
  value
}

# **************************
# end ----
# **************************
