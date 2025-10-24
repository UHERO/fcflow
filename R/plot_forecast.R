# *************************
# Plot forecasts
# *************************

#' Plot Forecast Histories
#'
#' Combines the current forecast, previous vintage, and history to create
#' interactive HTML plots (and optionally annual summaries) for selected
#' series.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param forecast Optional xts/tibble override for the current forecast.
#' @param comparison_forecast Optional override for the previous forecast data.
#' @param history Optional override for historical data.
#'
#' @return Invisible list containing the plot objects, combined data, and
#'   the file path of the rendered HTML report.
#' @export
plot_forecast <- function(
  cfg = load_forecast_cfg(),
  forecast = NULL,
  comparison_forecast = NULL,
  history = NULL
) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  comp_vint <- require_cfg(cfg, c("vintages", "comp"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  out_dir <- require_cfg(cfg, c("paths", "output"))
  out_fig_dir <- require_cfg(cfg, c("paths", "figures"))
  toursol_plot_list <- require_cfg(cfg, c("paths", "toursol_plot_list"))
  qsol_plot_list <- require_cfg(cfg, c("paths", "qsol_plot_list"))

  plot_cfg <- require_cfg(cfg, c("plot_forecast"))
  save_output <- require_cfg(plot_cfg, c("save_output"))
  preview_html <- require_cfg(plot_cfg, c("preview_html"))
  tourplot <- require_cfg(plot_cfg, c("tourplot"))
  jobonly <- require_cfg(plot_cfg, c("jobonly"))
  ypjonly <- require_cfg(plot_cfg, c("ypjonly"))
  yonly <- require_cfg(plot_cfg, c("yonly"))
  annual <- require_cfg(plot_cfg, c("annual"))
  load_curr_vint <- require_cfg(plot_cfg, c("load_curr_vint"))
  load_comp_vint <- require_cfg(plot_cfg, c("load_comp_vint"))
  load_history <- require_cfg(plot_cfg, c("load_history"))
  plot_width <- require_cfg(plot_cfg, c("plot_width"))
  plot_height <- require_cfg(plot_cfg, c("plot_height"))
  yoy_growth <- require_cfg(plot_cfg, c("yoy_growth"))
  comparison_forecast_file <- require_cfg(
    plot_cfg,
    c("comparison_forecast_file")
  )
  forecast_file <- require_cfg(cfg, c("solve_model", "forecast_file"))
  history_file <- require_cfg(cfg, c("data_model", "data_model_file"))

  short <- require_cfg(plot_cfg, c("short"))

  range_cfg <- if (isTRUE(short)) {
    require_cfg(plot_cfg, c("ranges", "short"))
  } else {
    require_cfg(plot_cfg, c("ranges", "long"))
  }
  plot_start_val <- require_cfg(range_cfg, c("plot_start"))
  plot_end_val <- require_cfg(range_cfg, c("plot_end"))
  tab_start_val <- require_cfg(range_cfg, c("tab_start"))
  tab_end_val <- require_cfg(range_cfg, c("tab_end"))
  show_table <- require_cfg(range_cfg, c("show_table"))

  plot_start <- lubridate::parse_date_time(plot_start_val, c("yq", "ymd")) %>%
    lubridate::as_date()
  plot_end <- lubridate::parse_date_time(plot_end_val, c("yq", "ymd")) %>%
    lubridate::as_date()
  tab_start <- lubridate::parse_date_time(tab_start_val, c("yq", "ymd")) %>%
    lubridate::as_date()
  tab_end <- lubridate::parse_date_time(tab_end_val, c("yq", "ymd")) %>%
    lubridate::as_date()

  namepre <- if (isTRUE(tourplot)) "tour" else "macro"
  plot_name <- if (isTRUE(short)) {
    paste0(namepre, "-sr-", curr_vint, "-vs-", comp_vint, ".html")
  } else {
    paste0(namepre, "-lr-", curr_vint, "-vs-", comp_vint, ".html")
  }
  plot_loc <- here::here(out_fig_dir, plot_name)

  # choose the appropriate plot list (tourism vs macro) based on the config
  if (isTRUE(tourplot)) {
    script_result_env <- run_script_with_args(
      path = here::here(toursol_plot_list)
    )
    # retrieve the modified data from the script's environment
    plot_list <- script_result_env$plot_list
  } else {
    script_result_env <- run_script_with_args(
      path = here::here(qsol_plot_list)
    )
    # retrieve the modified data from the script's environment
    plot_list <- script_result_env$plot_list
  }

  # optional filters let users focus on job-related, income-related, or custom subsets of series
  if (isTRUE(jobonly)) {
    plot_list <- plot_list %>% stringr::str_subset("^E")
  }
  if (isTRUE(ypjonly)) {
    plot_list <- plot_list %>% stringr::str_subset("^YPJ")
  }
  if (isTRUE(yonly)) {
    plot_list <- plot_list %>% stringr::str_subset("^Y")
  }

  # load the current-vintage forecast (or use the override passed into the function)
  if (isTRUE(load_curr_vint)) {
    message("Load current forecast...")
    forecast <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        forecast_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_SOL"))
  } else if (!is.null(forecast)) {
    forecast <- forecast %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_SOL"))
  } else {
    stop("Current forecast data must be supplied when load_curr_vint is FALSE.")
  }

  # comparison forecast gives the “old” path in the plots
  if (isTRUE(load_comp_vint)) {
    message("Load comparison forecast...")
    comparison_forecast <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        comparison_forecast_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(
        id = stringr::str_glue("{id}_{comp_vint}") %>% as.character()
      )
  } else if (is.null(comparison_forecast)) {
    stop(
      "Previous forecast data must be supplied when load_comp_vint is FALSE."
    )
  }

  # history supplies the actual data points so plots can show how forecasts converge to reality
  if (isTRUE(load_history)) {
    message("Load history...")
    history <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        history_file %>% stringr::str_replace(".csv$", ".RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::filter(
        .data$id %in%
          (dplyr::pull(forecast, .data$id) %>%
            stringr::str_replace("_SOL", ""))
      ) %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_Q"))
  } else if (is.null(history)) {
    stop("Historical data must be supplied when load_history is FALSE.")
  }

  # assemble current forecast, previous forecast, and history into one tidy object for plotting
  fcst_comp_hist <- tsbox::ts_c(forecast, comparison_forecast, history)
  plot_list <- unique(fcst_comp_hist$id) %>%
    tibble::as_tibble_col() %>%
    dplyr::mutate(
      selector = stringr::str_detect(
        .data$value,
        stringr::str_flatten(plot_list, collapse = "|")
      )
    ) %>%
    dplyr::filter(.data$selector) %>%
    dplyr::pull(.data$value) %>%
    stringr::str_remove_all("(_SOL|_Q|_[0-9]{2,4}Q[1-4])$")

  if (isTRUE(save_output)) {
    message("Save plot data...")
    fcst_comp_hist %>%
      dplyr::arrange(.data$id, .data$time) %>%
      tsbox::ts_wide() %>%
      readr::write_csv(here::here(
        out_dir,
        stringr::str_glue("fcst_comp_hist.csv")
      ))
  }

  message("Generate plots...")
  # generate one interactive chart per mnemonic family defined in the plot list
  plot_out <- plot_list %>%
    purrr::map(
      ~ fcutils::plot_fc(
        fcst_comp_hist %>%
          dplyr::filter(stringr::str_detect(.data$id, stringr::str_c("^", .x))),
        rng_start = plot_start,
        rng_end = as.character(plot_end),
        width = plot_width,
        height = plot_height,
        yoy_gr = yoy_growth,
        table_start = tab_start,
        table_end = tab_end,
        add_table = show_table
      )
    )

  if (isTRUE(save_output)) {
    message("Save plots...")
    # save the underlying data table and render the HTML dashboards for sharing
    plot_out %>%
      fcutils::save_plot_list(save_loc = plot_loc)

    if (isTRUE(preview_html) && interactive()) {
      utils::browseURL(plot_loc)
    }
  }

  if (isTRUE(annual)) {
    message("Generate annual plots...")
    fcst_comp_hist_a <- fcst_comp_hist %>%
      fcutils::aggr(conv_type = "uhero")

    plot_out_a <- plot_list %>%
      purrr::map(
        ~ fcutils::plot_fc(
          fcst_comp_hist_a %>%
            dplyr::filter(stringr::str_detect(
              .data$id,
              stringr::str_c("^", .x)
            )),
          rng_start = plot_start,
          rng_end = as.character(plot_end),
          width = plot_width,
          height = plot_height,
          yoy_gr = yoy_growth,
          table_start = tab_start,
          table_end = tab_end,
          add_table = show_table
        )
      )

    if (isTRUE(save_output)) {
      message("Save annual plots...")
      plot_out_a %>%
        fcutils::save_plot_list(
          save_loc = stringr::str_replace(plot_loc, ".html", "_A.html")
        )

      if (isTRUE(preview_html) && interactive()) {
        utils::browseURL(stringr::str_replace(plot_loc, ".html", "_A.html"))
      }
    }
  }

  invisible(
    list(
      plots = plot_out,
      plot_path = plot_loc,
      data = fcst_comp_hist,
      comparison_forecast = comparison_forecast,
      history = history
    )
  )
}

if (identical(environment(), globalenv())) {
  plot_forecast()
}

# **************************
# end ----
# **************************
