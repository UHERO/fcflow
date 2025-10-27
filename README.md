# fcflow

Forecast workflow toolkit for UHERO’s quarterly macro model. The package wraps
data preparation, equation management, estimation, simulation, and reporting
into a set of reproducible R functions that can be called from scripts, targets
pipelines, or interactively.

<!-- badges: start -->
[![R-CMD-check](https://github.com/UHERO/fcflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UHERO/fcflow/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

```r
# install dependencies declared in DESCRIPTION (fcutils, bimets, etc.)
remotes::install_github("UHERO/fcflow")

# optional: restore the exact development environment
renv::restore()
```

Development scripts assume the project is opened in RStudio (or similar) with
the working directory set to the repository root so that `here::here()` resolves
correctly.

## Repository layout

| Path | Purpose |
|------|---------|
| `R/` | Exported functions such as `make_data_main()`, `solve_model()`, `select_model()`. |
| `inst/extdata/config/` | Template YAML configurations. Copy `config.yml` into `config/` and edit for a specific vintage. |
| `inst/extfuns/` | User-editable helper scripts for wrangling, extending history, importing add-factors, and creating plot lists. |
| `data/raw`, `data/processed`, `equations`, `output/figures` | Default directories referenced in the config (`paths` node). |
| `tools/pack_dev.R` | Notes on setting up the development environment. |

## Configuration

Use `fcflow::load_forecast_cfg()` to read the active configuration. The loader
searches in this order:

1. The explicit `path` argument.
2. `Sys.getenv("MODDEV_CONFIG")`.
3. `here::here("config", "config.yml")`.
4. The packaged default (`inst/extdata/config/config.yml` installed with
   `fcflow`).

Key sections in the YAML file:

- `vintages`: current (`curr`) and comparison (`comp`) vintage labels.
- `paths`: locations of raw/processed data, equations, figure output, and the
  helper scripts that are sourced via `run_script_with_args()`. Update these
  entries if you relocate the scripts from `inst/extfuns/`.
- `data_main`, `data_model`, `make_model`, `solve_model`, `plot_forecast`,
  `data_gets`, `select_model`: module-specific settings controlling filenames,
  toggles (e.g., `update_equations`), date ranges, and save locations.
- `combine_equations`: which `*_eq.txt` files to stitch together and where to
  cache the combined `model_equations.txt` / `.RDS`.

## Core workflow functions

All exported functions accept the configuration object produced by
`load_forecast_cfg()` and can take pre-loaded data objects to avoid redundant
disk I/O.

1. `combine_equations(cfg)`: reads the BIMETS equation snippets located under
   `paths$equations`, filters by `combine_equations$equations_subset`, and
   produces `model_equations.txt` and `.RDS`.
2. `make_data_main(cfg, indicators = NULL)`: pulls history (either from disk or
   via `fcutils::get_series_exp()`), applies `extend_script` and
   `wrangl_script`, and writes `data_main.csv/.RDS` to `paths$processed`.
3. `make_data_model(cfg, data_main, model_equations)`: restricts the master set
   to the variables required by the model and chains in pseudo-exogenous series
   via `impexf_script` (default `inst/extfuns/import_existing_forecast.R`).
4. `make_model(cfg, data_model, model_equations)`: estimates behavioral
   equations (optionally discovering or forcing TSRANGE), initializes zero add
   factors, and records the ragged edge metadata.
5. `solve_model(cfg, estimated_equations, exog_range, add0_factors)`: sources
   the add-factor script (`paths$addfac_script`), runs the BIMETS simulation,
   and saves `forecast.csv/.RDS` plus the add-factor log.
6. `plot_forecast(cfg, forecast, comparison_forecast, history)`: assembles
   history/current/comparison vintages, selects the plot universe using the
   tourism or macro plot list script, and writes interactive HTML charts to
   `paths$figures`.

Each function returns a named list so that outputs can be piped into the next
step without needing to touch the filesystem.

## Model selection and GETS

- `make_data_gets(cfg, data_main, existing_forecast)`: chains the historical
  dataset with the pseudo-forecast to create the extended panel used for GETS.
  Outputs `data_main_extended.csv/.RDS`.
- `select_model(cfg, model_select_data, indicators)`: orchestrates the GETS /
  ISAT workflow defined in `select_model` config nodes, saves a candidate
  equation (`model_eq.txt`), compiles and estimates it via BIMETS, and produces
  diagnostic plots in `paths$figures`.

Both functions honor the same path conventions as the core workflow and rely on
`inst/extfuns/model_selection.R` for interactive experimentation.

## Helper scripts

The scripts under `inst/extfuns/` are templates that get sourced at runtime:

- `wrangle_data.R`: apply data adjustments that used to live in add-factor
  files (seasonal dummy tweaks, zeroing pandemic periods, derived ratios, etc.).
- `extend_qmain_history.R`: splice additional history from archived AREMOS TSVs.
- `import_existing_forecast.R`: load QSOL/ASOL files, disaggregate annual
  series, construct real values, and ensure exogenous drivers exist for every
  equation.
- `add_qmod.R`: customize add factors prior to solving the model.
- `qsol_plot_list.R` / `toursol_plot_list.R`: define which mnemonics appear in
  macro vs. tourism dashboards.
- `forecast_work.R`: convenience script to run any subset of the workflow by
  toggling booleans in `run_steps`.

Copy these files into the locations referenced by `paths` (or point the config
to the copies inside `inst/extfuns/`) before running the package functions.

## Typical run

```r
library(fcflow)

cfg <- load_forecast_cfg()        # read config/config.yml or fallback
equations <- combine_equations(cfg)
data_main <- make_data_main(cfg)$data_main
data_model <- make_data_model(cfg,
                              data_main = data_main,
                              model_equations = equations$model_equations)
est <- make_model(cfg,
                  data_model = data_model$data_model,
                  model_equations = data_model$model_equations)
fcst <- solve_model(cfg,
                    estimated_equations = est$estimated_equations,
                    exog_range = est$exog_range,
                    add0_factors = est$add0_factors)
plots <- plot_forecast(cfg, forecast = fcst$forecast)
```

Swap any of the intermediate arguments with `NULL` to fall back to previously
saved artifacts on disk.

## Development

- Use `renv::restore()` to install compatible dependency versions.
- Run `devtools::document()`, `devtools::check()`, and `pkgdown::build_site()`
  before opening a PR.
- The GitHub action `R-CMD-check` mirrors the standard `devtools::check()` run,
  so ensure it passes locally when possible.

## Support / Questions

For repo-specific questions, open an issue on GitHub. To request additions to
`fcutils` or the underlying BIMETS equations, coordinate with the UHERO
forecast team so the config and helper scripts stay in sync with the latest
workflow changes.
