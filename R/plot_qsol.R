# *************************
# Plot QMOD forecasts
# *************************

#' Plot Forecast Histories
#'
#' Combines the current forecast, previous vintage, and history to create
#' interactive HTML plots (and optionally annual summaries) for selected
#' series.
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @param fcst Optional xts/tibble override for the current forecast.
#' @param prev_fcst Optional override for the previous forecast data.
#' @param history Optional override for historical data.
#'
#' @return Invisible list containing the plot objects, combined data, and
#'   the file path of the rendered HTML report.
#' @export
plot_qsol <- function(
  cfg = load_forecast_cfg(),
  fcst = NULL,
  prev_fcst = NULL,
  history = NULL
) {
  curr_vint <- require_cfg(cfg, c("vintages", "curr"))
  prev_vint <- require_cfg(cfg, c("vintages", "prev"))
  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  out_dir <- require_cfg(cfg, c("paths", "output"))
  out_fig_dir <- require_cfg(cfg, c("paths", "figures"))

  plot_cfg <- require_cfg(cfg, c("plot_qsol"))
  save_outputs <- require_cfg(plot_cfg, c("save_outputs"))
  preview_html <- require_cfg(plot_cfg, c("preview_html"))
  tourplot <- require_cfg(plot_cfg, c("tourplot"))
  jobonly <- require_cfg(plot_cfg, c("jobonly"))
  ypjonly <- require_cfg(plot_cfg, c("ypjonly"))
  yonly <- require_cfg(plot_cfg, c("yonly"))
  annual <- require_cfg(plot_cfg, c("annual"))
  load_curr_vint <- require_cfg(plot_cfg, c("load_curr_vint"))
  load_prev_vint <- require_cfg(plot_cfg, c("load_prev_vint"))
  load_history <- require_cfg(plot_cfg, c("load_history"))
  plot_width <- require_cfg(plot_cfg, c("plot_width"))
  plot_height <- require_cfg(plot_cfg, c("plot_height"))
  yoy_growth <- require_cfg(plot_cfg, c("yoy_growth"))

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
    paste0(namepre, "-sr-", curr_vint, "-vs-", prev_vint, ".html")
  } else {
    paste0(namepre, "-lr-", curr_vint, "-vs-", prev_vint, ".html")
  }
  plot_loc <- here::here(out_fig_dir, plot_name)

  # choose the appropriate plot list (tourism vs macro) based on the config
  if (isTRUE(tourplot)) {
    source(here::here("lists", "toursol_plots_list.txt"))
  } else {
    source(here::here("lists", "qsol_plots_list.txt"))
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
    fcst <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("fcst_{curr_vint}.RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_SOL"))
  } else if (!is.null(fcst)) {
    fcst <- fcst %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_SOL"))
  } else {
    stop("Current forecast data must be supplied when load_curr_vint is FALSE.")
  }

  # previous forecast gives the “old” path for comparison in the plots
  if (isTRUE(load_prev_vint)) {
    message("Load previous forecast...")
    prev_fcst <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("fcst_{prev_vint}.RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(
        id = stringr::str_glue("{id}_{prev_vint}") %>% as.character()
      )
  } else if (!is.null(prev_fcst)) {
    prev_fcst <- prev_fcst %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(
        id = stringr::str_glue("{id}_{prev_vint}") %>% as.character()
      )
  } else {
    stop(
      "Previous forecast data must be supplied when load_prev_vint is FALSE."
    )
  }

  # history supplies the actual data points so plots can show how forecasts converge to reality
  if (isTRUE(load_history)) {
    message("Load history...")
    history <- readRDS(
      file = here::here(
        dat_prcsd_dir,
        stringr::str_glue("data_qmod_{curr_vint}.RDS")
      )
    ) %>%
      tsbox::ts_tbl() %>%
      dplyr::filter(
        .data$id %in%
          (dplyr::pull(fcst, .data$id) %>%
            stringr::str_replace("_SOL", ""))
      ) %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_Q"))
  } else if (!is.null(history)) {
    history <- history %>%
      tsbox::ts_tbl() %>%
      dplyr::mutate(id = stringr::str_replace(.data$id, "_Q$", "")) %>%
      dplyr::filter(
        .data$id %in%
          (dplyr::pull(fcst, .data$id) %>%
            stringr::str_replace("_SOL", ""))
      ) %>%
      dplyr::mutate(id = stringr::str_c(.data$id, "_Q"))
  } else {
    stop("Historical data must be supplied when load_history is FALSE.")
  }

  # assemble current forecast, previous forecast, and history into one tidy object for plotting
  fcst_prev_hist <- tsbox::ts_c(fcst, prev_fcst, history)
  plot_list <- unique(fcst_prev_hist$id) %>%
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

  if (isTRUE(save_outputs)) {
    message("Save plot data...")
    fcst_prev_hist %>%
      dplyr::arrange(.data$id, .data$time) %>%
      tsbox::ts_wide() %>%
      readr::write_csv(here::here(
        out_dir,
        stringr::str_glue("fcst_prev_hist_{curr_vint}.csv")
      ))
  }

  message("Generate plots...")
  # generate one interactive chart per mnemonic family defined in the plot list
  plot_out <- plot_list %>%
    purrr::map(
      ~ fcutils::plot_fc(
        fcst_prev_hist %>%
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

  if (isTRUE(save_outputs)) {
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
    fcst_prev_hist_a <- fcst_prev_hist %>%
      fcutils::aggr(conv_type = "uhero")

    plot_out_a <- plot_list %>%
      purrr::map(
        ~ fcutils::plot_fc(
          fcst_prev_hist_a %>%
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

    if (isTRUE(save_outputs)) {
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
      data = fcst_prev_hist,
      plot_path = plot_loc
    )
  )
}

if (identical(environment(), globalenv())) {
  plot_qsol()
}

# **************************
# end ----
# **************************
