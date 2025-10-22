# **************************
# GETS model development ----
# **************************

#' Run GETS-based model selection using configuration settings.
#'
#' Executes the GETS workflow for a single equation, sourcing all parameters
#' from `config.yml`. The function can optionally accept preloaded data to avoid
#' disk reads and returns key artefacts invisibly for downstream use.
#' set yvar_name, xvar_list and other parameters
#' dataset contains extended history and pseudo-forecast
#' model equation saved in text file ready to be copied
#' plot compares new forecast with pseudo-forecast
#'
#' @param cfg Configuration list produced by [load_forecast_cfg()].
#' @param mod_select_data Optional extended dataset from [make_data_gets()] (wide tibble,
#'   xts, or tsbox-compatible object). When `NULL`, the data are read from disk.
#' @param indicators Optional indicator variables (xts). When `NULL`, the file
#'   specified in the configuration is loaded.
#'
#' @return Invisibly returns a list containing fitted models, data, and plots.
#' @export
gets_model_select <- function(
  cfg = load_forecast_cfg(),
  mod_select_data = NULL,
  indicators = NULL
) {
  # some gets functions don't work without loading the namespace
  requireNamespace("gets", quietly = TRUE)

  # helper to build diagnostic test configuration
  build_diag <- function(node) {
    if (is.null(node)) {
      return(NULL)
    }

    lag_val <- node$lag
    pval_val <- node$pval

    if (is.null(lag_val) && is.null(pval_val)) {
      return(NULL)
    }

    list(
      lag = if (is.null(lag_val)) NULL else as.numeric(lag_val),
      pval = if (is.null(pval_val)) NULL else as.numeric(pval_val)
    )
  }

  dat_prcsd_dir <- require_cfg(cfg, c("paths", "processed"))
  dat_raw_dir <- require_cfg(cfg, c("paths", "raw"))
  eqn_dir <- require_cfg(cfg, c("paths", "equations"))
  out_fig_dir <- require_cfg(cfg, c("paths", "figures"))
  indicators_file <- require_cfg(cfg, c("data_qmain", "indicators_file"))

  bank_start <- require_cfg(cfg, c("constants", "bank_start")) %>%
    fcutils::to_ymd()
  bank_end <- require_cfg(cfg, c("constants", "bank_end")) %>%
    fcutils::to_ymd()

  # candidate variables (logs: L_, diffs: D_, logdiffs: DL_, parsed by model_equation())
  # derived variables (interactions, ratios, etc.) defined in make_data_gets.R
  yvar_name <- require_cfg(cfg, c("gets_model_select", "yvar"))
  xvar_list <- magrittr::extract2(cfg, c("gets_model_select", "xvars"))

  # deterministic variables (e.g., constant, time trend, seasonal dummies)
  ivar_list <- magrittr::extract2(cfg, c("gets_model_select", "deterministic"))

  mod_list <- c(yvar_name, xvar_list)

  # variables to keep in final model
  keep_vars <- magrittr::extract2(cfg, c("gets_model_select", "keep_vars"))

  max_lag <- as.integer(require_cfg(cfg, c("gets_model_select", "max_lag")))
  second_pass <- isTRUE(require_cfg(cfg, c("gets_model_select", "second_pass")))
  save_eq <- isTRUE(require_cfg(cfg, c("gets_model_select", "save_equation")))

  mselect_start <- require_cfg(
    cfg,
    c("gets_model_select", "mselect_range", "start")
  ) %>%
    fcutils::to_ymd()
  mselect_end <- require_cfg(
    cfg,
    c("gets_model_select", "mselect_range", "end")
  ) %>%
    fcutils::to_ymd()

  est_start <- require_cfg(
    cfg,
    c("gets_model_select", "estimation_range", "start")
  ) %>%
    fcutils::to_ymd()
  est_end <- require_cfg(
    cfg,
    c("gets_model_select", "estimation_range", "end")
  ) %>%
    fcutils::to_ymd()

  fcst_start <- require_cfg(
    cfg,
    c("gets_model_select", "forecast_range", "start")
  ) %>%
    fcutils::to_ymd()
  fcst_end <- require_cfg(
    cfg,
    c("gets_model_select", "forecast_range", "end")
  ) %>%
    fcutils::to_ymd()

  # diagnostic tests for serieal correlation and arch effects
  diag_cfg <- magrittr::extract2(
    cfg,
    c("gets_model_select", "diagnostic_tests")
  )
  ar.LjungB_test <- build_diag(diag_cfg$ar_ljung_box)
  arch.LjungB_test <- build_diag(diag_cfg$arch_ljung_box)
  t_pval <- require_cfg(
    cfg,
    c("gets_model_select", "diagnostic_tests", "t_pval")
  )

  plot_cfg <- require_cfg(cfg, c("gets_model_select", "plot"))
  plot_filename <- plot_cfg$filename
  if (is.null(plot_filename) || !nzchar(plot_filename)) {
    plot_filename <- "plot_out.html"
  }
  plot_window_years <- plot_cfg$window_years
  if (is.null(plot_window_years)) {
    plot_window_years <- 15
  }
  plot_preview_html <- isTRUE(plot_cfg$preview_html)

  mod_select_data_file <- require_cfg(
    cfg,
    c(
      "make_data_gets",
      "mod_select_data_file"
    )
  )

  # **************************
  # get data ----
  # **************************

  hist_q_in <- if (is.null(mod_select_data)) {
    readr::read_csv(
      file = here::here(dat_prcsd_dir, mod_select_data_file),
      show_col_types = FALSE
    ) %>%
      tsbox::ts_long()
  } else {
    mod_select_data %>%
      tsbox::ts_tbl()
  }

  hist_q <- hist_q_in %>%
    dplyr::filter(stringr::str_detect(
      .data$id,
      "^IIS|^SIS|^CONST|^TREND|^IQ|^DUM|^SEASON|^QV",
      negate = TRUE
    )) %>%
    dplyr::filter(
      .data$id %in%
        stringr::str_replace_all(
          mod_list,
          c("^L_" = "", "^D_" = "", "^DL_" = "")
        )
    ) %>%
    tsbox::ts_span(bank_start, bank_end)

  # **************************
  # transform variables ----
  # **************************

  # take the log of all series (some will produce NaNs)
  hist_ql <- hist_q %>%
    dplyr::mutate(value = log(.data$value)) %>%
    dplyr::mutate(id = stringr::str_c("L_", .data$id))

  hist_qd <- hist_q %>%
    tsbox::ts_diff() %>%
    dplyr::mutate(id = stringr::str_c("D_", .data$id))

  hist_qdl <- hist_ql %>%
    tsbox::ts_diff() %>%
    dplyr::mutate(id = stringr::str_c("D", .data$id))

  # **************************
  # indicator variables ----
  # **************************

  indicator_vars <- if (is.null(indicators)) {
    readRDS(here::here(dat_raw_dir, indicators_file)) %>%
      tsbox::ts_tbl()
  } else {
    indicators %>%
      tsbox::ts_tbl()
  }

  # **************************
  # combine data ----
  # **************************

  hist_q_mod <- hist_q %>%
    tsbox::ts_c(hist_ql) %>%
    tsbox::ts_c(hist_qd) %>%
    tsbox::ts_c(hist_qdl) %>%
    tsbox::ts_c(indicator_vars) %>%
    tsbox::ts_wide()

  # **************************
  # subset of data considered for model selection ----
  # **************************

  # use a subset of observations for model selection
  hist_q_mselect <- hist_q_mod %>%
    tsbox::ts_long() %>%
    tsbox::ts_span(mselect_start, mselect_end)

  # select candidate target variables
  yvar_0 <- hist_q_mselect %>%
    dplyr::filter(.data$id == yvar_name)

  # transform to xts
  yvar_xts <- yvar_0 %>%
    tsbox::ts_xts()

  # are any other regressors considered for the model?
  if (!is.null(xvar_list)) {
    # select candidate predictor variables
    xvar_0 <- hist_q_mselect %>%
      dplyr::filter(.data$id %in% xvar_list)

    # generate lags of selected predictors
    xvar_lags <- purrr::map(
      0:max_lag,
      ~ tsbox::ts_lag(xvar_0, .x)
    ) %>%
      purrr::reduce(tsbox::ts_c)

    # combine all predictors
    if (!is.null(ivar_list)) {
      # select candidate predictor variables
      xvar_all <- xvar_lags %>%
        # include deterministic variables
        tsbox::ts_c(
          hist_q_mselect %>%
            dplyr::filter(.data$id %in% ivar_list)
        )
    } else {
      # only predictors
      xvar_all <- xvar_lags
    }
  } else {
    # combine all predictors
    if (!is.null(ivar_list)) {
      # include deterministic variables
      # no additional regressors are considered
      xvar_all <- hist_q_mselect %>%
        dplyr::filter(.data$id %in% ivar_list)
    } else {
      # stop script with error
      stop("The GUM has to contain at least one predictor.")
    }
  }

  xvar_xts <- xvar_all %>%
    tsbox::ts_xts()

  # **************************
  # select model with gets ----
  # **************************

  # modeling with gets
  # https://cran.r-project.org/web/packages/gets/index.html

  # formulate a general unrestricted model ----
  # NA-s are automatically eliminated from the data
  gum_model <- gets::arx(
    y = yvar_xts,
    mc = FALSE,
    ar = 1:max_lag,
    mxreg = xvar_xts
  )
  gum_model
  gets::plot.gets(gum_model)
  gum_model %>%
    stats::residuals() %>%
    graphics::hist()
  gum_model %>%
    stats::residuals() %>%
    tsbox::ts_ts() %>%
    stats::acf()

  # save the data associated with the GUM
  gum_data <- fcutils::extract_data(gum_model, yvar_name)

  # identify outliers in the GUM ----
  isat_model <- gets::isat(
    y = gum_data[, 1],
    mc = FALSE,
    mxreg = gum_data[, -1],
    ar.LjungB = ar.LjungB_test,
    arch.LjungB = arch.LjungB_test,
    iis = TRUE,
    sis = TRUE,
    plot = TRUE
  )
  isat_model
  gets::plot.gets(isat_model)
  isat_model %>%
    stats::residuals() %>%
    graphics::hist()
  isat_model %>%
    stats::residuals() %>%
    tsbox::ts_ts() %>%
    stats::acf()

  # save the data associated with the GUM+outliers
  isat_data <- fcutils::extract_data(isat_model, yvar_name)

  # numbering of variables to keep
  keep_vars_nr <- if (is.null(keep_vars)) {
    NULL
  } else {
    which(isat_model$aux$mXnames %in% keep_vars)
  }

  # run the gets (general to specific) model selection algorithm ----
  # gets::getsm() does not work !!!
  gets_model <- getsm(
    isat_model,
    t.pval = t_pval,
    ar.LjungB = ar.LjungB_test,
    arch.LjungB = arch.LjungB_test,
    keep = keep_vars_nr
  )
  gets_model
  gets::plot.gets(gets_model)
  gets_model %>%
    stats::residuals() %>%
    graphics::hist()
  gets_model %>%
    stats::residuals() %>%
    tsbox::ts_ts() %>%
    stats::acf()

  # check outliers in the residuals
  isat_res <- gets_model %>%
    stats::residuals() %>%
    gets::isat()
  isat_res
  gets::plot.gets(isat_res)

  # save the data associated with the specific model
  gets_data <- fcutils::extract_data(gets_model, yvar_name)

  # second pass through model selection ----
  if (isTRUE(second_pass)) {
    # check outliers in the relationship
    isat_rel <- gets::isat(
      y = gets_data[, 1],
      mc = FALSE,
      mxreg = gets_data[, -1],
      ar.LjungB = ar.LjungB_test,
      arch.LjungB = arch.LjungB_test,
      iis = TRUE,
      sis = TRUE,
      plot = TRUE
    )

    # save the data associated with the specific model incl. the outlier controls
    second_pass_data <- fcutils::extract_data(isat_rel, yvar_name)

    # re-estimate the specific model with outliers
    second_pass_outliers <- gets::arx(
      y = second_pass_data[, 1],
      mc = FALSE,
      mxreg = second_pass_data[, -1]
    )

    # numbering of variables to keep
    keep_vars_nr_2 <- if (is.null(keep_vars)) {
      NULL
    } else {
      which(second_pass_outliers$aux$mXnames %in% keep_vars)
    }

    # repeat gets model selection over specific model and outliers
    # gets::getsm() does not work !!!
    second_pass_model <- getsm(
      second_pass_outliers,
      t.pval = t_pval,
      ar.LjungB = ar.LjungB_test,
      arch.LjungB = arch.LjungB_test,
      keep = keep_vars_nr_2
    )
    second_pass_model
    gets::plot.gets(second_pass_model)
    second_pass_model %>%
      stats::residuals() %>%
      graphics::hist()
    second_pass_model %>%
      stats::residuals() %>%
      tsbox::ts_ts() %>%
      stats::acf()

    # check outliers in the residuals
    isat_res <- second_pass_model %>%
      stats::residuals() %>%
      gets::isat()
    isat_res
    gets::plot.gets(isat_res)

    # save the data associated with the re-specified specific model incl. the outlier controls
    gets_data <- fcutils::extract_data(second_pass_model, yvar_name)
  }

  # verify that no additional outliers arise due to greater model parsimony ----
  isat_check <- gets::isat(
    y = gets_data[, 1],
    mc = FALSE,
    mxreg = gets_data[, -1],
    ar.LjungB = ar.LjungB_test,
    arch.LjungB = arch.LjungB_test,
    iis = TRUE,
    sis = TRUE,
    plot = TRUE
  )
  isat_check
  gets::plot.gets(isat_check)

  # # this is just an illustration (does not pick up already modeled shocks)
  # # shocks to the constant implied by outliers, path of the constant
  # isat_path <- isatvar(isat_check)
  #
  # # plot of significant shifts in the constant
  # isattest(isat_check, plot = TRUE)
  #
  # # last value of the constant path (incorporating shifts)
  # # this will be the same as the estimated constant
  # # unless new shifts are identified in this iteration
  # const_last <- isat_path %>%
  #   ts_tbl() %>%
  #   filter(id == "const.path") %>%
  #   pull(value) %>%
  #   last()

  # move the constant to the first position among predictors
  gets_data <- gets_data %>%
    fcutils::conv_wide() %>%
    dplyr::relocate(dplyr::matches("CONST"), .after = "time") %>%
    fcutils::conv_xts()

  # re-estimate re-specified model via lm or arx
  gets_isat_lm <- stats::lm(
    stats::as.formula(stringr::str_c(yvar_name, " ~ . - 1")),
    data = gets_data
  )

  gets_isat_arx <- gets::arx(
    y = gets_data[, 1],
    mc = FALSE,
    mxreg = gets_data[, -1]
  )

  # **************************
  # eliminate zeros in the estimation sample ----
  # **************************

  # find all variables with only zeros in the estimation period (if shorter than model selection period)
  zero_vars <- gets_data %>%
    tsbox::ts_span(start = est_start, end = est_end) %>%
    tsbox::ts_tbl() %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(sum_vals = sum(.data$value), .groups = "drop") %>%
    dplyr::filter(.data$sum_vals == 0) %>%
    dplyr::pull(.data$id)

  # remove zero valued variables from the estimation sample
  est_data <- gets_data %>%
    tsbox::ts_span(start = est_start, end = est_end) %>%
    tsbox::ts_tbl() %>%
    tsbox::ts_wide() %>%
    dplyr::select(-dplyr::all_of(zero_vars)) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # re-estimate re-specified model via lm using the estimation sample
  est_lm <- stats::lm(
    stats::as.formula(stringr::str_c(yvar_name, " ~ . - 1")),
    data = est_data
  )

  # **************************
  # save model equation ----
  # **************************

  # look at estimated model and bimets model components
  fcutils::model_equation(est_lm)

  # save equations into text file? # save_eq == TRUE
  if (isTRUE(save_eq)) {
    sink(here::here(eqn_dir, "model_eq.txt"))
    cat("\n", "MODEL", "\n", sep = "")
    sink()

    # save equation
    sink(here::here(eqn_dir, "model_eq.txt"), append = TRUE)
    cat(stringr::str_glue(
      "

    {model_equation(est_lm)[2:5]}"
    ))
    sink()

    # end file with END
    sink(here::here(eqn_dir, "model_eq.txt"), append = TRUE)
    cat("\n\n", "END", "\n", sep = "")
    sink()
  }

  # **************************
  # compile model ----
  # **************************

  # load model from stored txt file
  model_eq <- bimets::LOAD_MODEL(
    modelFile = here::here(eqn_dir, "model_eq.txt")
  )

  # store variables in bimets format (no ragged edge: drop_na)
  hist_q_bimets <- hist_q_mod %>%
    tsbox::ts_long() %>%
    dplyr::filter(.data$id %in% c(model_eq$vendog, model_eq$vexog)) %>%
    tsbox::ts_wide() %>%
    tidyr::drop_na() %>%
    tsbox::ts_long() %>%
    tsbox::ts_tslist() %>%
    purrr::map(bimets::as.bimets)

  # add data to model
  model_eq_dat <- bimets::LOAD_MODEL_DATA(
    model_eq,
    c(hist_q_bimets)
  )

  # **************************
  # estimate model ----
  # **************************

  # determine range of history for estimation (+2 to accommodate first differnces)
  est_range <- model_eq_dat$modelData %>%
    magrittr::set_attr("class", c("tslist", "list")) %>%
    tsbox::ts_xts() %>%
    zoo::index() %>%
    magrittr::extract(c(max_lag + 2, length(.)))

  # limit estimation range?
  if (est_range[1] < est_start) {
    est_range[1] <- est_start
  }
  # limit estimation range?
  if (est_range[2] > est_end) {
    est_range[2] <- est_end
  }

  # determine last available date for Chow test and forecast
  fcst_end <- min(
    fcst_end,
    hist_q_mod %>%
      tsbox::ts_long() %>%
      dplyr::filter(
        .data$id %in% c(model_eq_dat$vexog, model_eq_dat$vendog)
      ) %>%
      tsbox::ts_summary() %>%
      dplyr::pull(.data$end) %>%
      min()
  )

  # save estimation results into text file? # save_eq == TRUE
  if (isTRUE(save_eq)) {
    sink(here::here(eqn_dir, "model_est.txt"))
    cat("\n", "ESTIMATION RESULTS", "\n", sep = "")
  }

  # estimate model
  if (
    lubridate::floor_date(fcst_end, "quarter") <=
      lubridate::floor_date(est_range[2], "quarter")
  ) {
    model_est <- bimets::ESTIMATE(
      model_eq_dat,
      eqList = model_eq_dat$vendog,
      TSRANGE = c(
        lubridate::year(est_range[1]),
        lubridate::quarter(est_range[1]),
        lubridate::year(est_range[2]),
        lubridate::quarter(est_range[2])
      ),
      quietly = FALSE
    )
  } else {
    model_est <- bimets::ESTIMATE(
      model_eq_dat,
      eqList = model_eq_dat$vendog,
      TSRANGE = c(
        lubridate::year(est_range[1]),
        lubridate::quarter(est_range[1]),
        lubridate::year(est_range[2]),
        lubridate::quarter(est_range[2])
      ),
      CHOWTEST = TRUE,
      CHOWPAR = c(
        lubridate::year(fcst_end),
        lubridate::quarter(fcst_end)
      ),
      quietly = FALSE
    )
  }

  # close txt file with estimation results
  if (isTRUE(save_eq)) {
    sink()
  }

  # **************************
  # simulate model ----
  # **************************

  # simulate model
  model_sim <- bimets::SIMULATE(
    model_est,
    simType = "FORECAST",
    TSRANGE = c(
      lubridate::year(fcst_start),
      lubridate::quarter(fcst_start),
      lubridate::year(fcst_end),
      lubridate::quarter(fcst_end)
    ),
    simConvergence = 0.00001,
    simIterLimit = 100,
    quietly = FALSE
  )

  # extract forecast
  model_fcst <- model_sim$simulation %>%
    magrittr::extract(model_sim$vendog) %>%
    magrittr::set_attr("class", c("tslist", "list")) %>%
    tsbox::ts_tbl() %>%
    dplyr::mutate(
      id = stringr::str_c(model_sim$vendog, "_SOL"),
      .before = "time"
    )

  # combine history and forecast for plot
  plot_data_fcst <- tsbox::ts_c(
    tsbox::ts_long(hist_q_mod) %>%
      dplyr::filter(.data$id %in% model_sim$vendog),
    model_fcst
  ) %>%
    tsbox::ts_wide() %>%
    dplyr::slice(which(
      !is.na(!!rlang::sym(model_sim$vendog)) |
        !is.na(!!rlang::sym(stringr::str_glue("{model_sim$vendog}_SOL")))
    ))

  # generate plots
  plot_out <- fcutils::plot_comp_2(
    plot_data_fcst %>% tsbox::ts_long(),
    rng_start = as.character(Sys.Date() - lubridate::years(plot_window_years)),
    rng_end = fcst_end %>% as.character(),
    height = 400,
    width = 800
  )

  fcutils::save_plot_list(
    plot_out,
    save_loc = here::here(out_fig_dir, plot_filename)
  )

  if (isTRUE(plot_preview_html)) {
    utils::browseURL(here::here(out_fig_dir, plot_filename))
  }

  invisible(
    list(
      gum_model = gum_model,
      isat_model = isat_model,
      gets_model = gets_model,
      isat_check = isat_check,
      gets_isat_lm = gets_isat_lm,
      gets_isat_arx = gets_isat_arx,
      est_lm = est_lm,
      model_est = model_est,
      model_sim = model_sim,
      model_fcst = model_fcst,
      plot = plot_out,
      data = list(
        hist_q_mod = hist_q_mod,
        gets_data = gets_data,
        est_data = est_data
      ),
      outputs = list(
        plot_path = here::here(out_fig_dir, plot_filename),
        preview_html = plot_preview_html
      )
    )
  )
}

if (identical(environment(), globalenv())) {
  gets_model_select()
}

# **************************
# end ----
# **************************
