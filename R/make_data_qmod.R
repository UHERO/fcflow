# *************************
# Prepare data for QMOD estimation and simulation
# *************************

#' Prepare Model-Ready Data
#'
#' Filters the master dataset to the variables required by QMOD, adds external
#' pseudo-exogenous series from existing forecast, and saves model inputs.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param data_qmain Optional preloaded master dataset.
#' @param equations_qmod Optional preloaded model object.
#'
#' @return Invisible list containing processed data, previous forecasts,
#'   equations, and the extension range.
#' @export
make_data_qmod <- function(
  cfg = load_forecast_cfg(),
  data_qmain = NULL,
  equations_qmod = NULL
) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  prev_vint <- require_cfg(cfg, c("vintages", "prev"))
  update_qmain <- require_cfg(cfg, c("make_data_qmod", "update_qmain"))
  update_equations <- require_cfg(cfg, c("make_data_qmod", "update_equations"))
  ext_start <- require_cfg(cfg, c("make_data_qmod", "ext_start"))
  ext_end <- require_cfg(cfg, c("make_data_qmod", "ext_end"))
  save_outputs <- require_cfg(cfg, c("make_data_qmod", "save_outputs"))

  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  eqn_dir <- require_cfg(cfg, c("paths", "equations"))

  # parse the extension start and end dates
  ext_start <- lubridate::parse_date_time(ext_start, c("yq", "ymd")) %>%
    lubridate::as_date()
  ext_end <- lubridate::parse_date_time(ext_end, c("yq", "ymd")) %>%
    lubridate::as_date()

  ext_tsrange <- fcutils::p(ext_start, ext_end)

  if (is.null(equations_qmod)) {
    if (isTRUE(update_equations)) {
      equations_qmod <- combine_equations(cfg = cfg)
    } else {
      equations_qmod <- readRDS(
        file = here::here(
          eqn_dir,
          stringr::str_glue("equations_qmod_{curr_vint}.RDS")
        )
      )
    }
  }

  if (is.null(data_qmain)) {
    if (isTRUE(update_qmain)) {
      data_qmain_xts <- make_data_qmain(cfg = cfg)
    } else {
      data_qmain_xts <- readRDS(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_glue("data_qmain_{curr_vint}.RDS")
        )
      )
    }
  } else {
    data_qmain_xts <- data_qmain
  }

  varlist_qmod <- c(equations_qmod$vendog, equations_qmod$vexog) %>%
    unique()

  data_qmod_xts <- data_qmain_xts %>%
    tsbox::ts_pick(varlist_qmod)

  existing_forecast <- import_existing_fcst(
    dat_raw_dir = dat_raw_dir,
    dat_prcsd_dir = dat_prcsd_dir,
    equations_qmod = equations_qmod,
    data_qmod_xts = data_qmod_xts,
    save_outputs = save_outputs
  )

  data_existing_fcst_xts <- existing_forecast$data_existing_fcst
  exog_list <- existing_forecast$exog_list

  # copy the pseudo-exogenous paths into the main data set over the simulation range
  data_qmod_xts[ext_tsrange, exog_list] <- data_existing_fcst_xts[
    ext_tsrange,
    exog_list
  ]

  if (isTRUE(save_outputs)) {
    saveRDS(
      data_qmod_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_qmod_{curr_vint}.RDS")
      )
    )

    data_qmod_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_glue("data_qmod_{curr_vint}.csv")
        )
      )
  }

  invisible(
    list(
      data_qmod = data_qmod_xts,
      data_existing_fcst = data_existing_fcst_xts,
      equations = equations_qmod,
      data_qmain = data_qmain_xts,
      ext_tsrange = ext_tsrange
    )
  )
}

if (identical(environment(), globalenv())) {
  make_data_qmod()
}

# **************************
# end ----
# **************************
