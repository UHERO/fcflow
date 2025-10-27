# **************************
# Import Existing Forecast
# **************************

#' Import Existing Forecast
#'
#' Loads quarterly and annual existing forecasts,
#' and constructs quarterly pseudo-exogenous series required by the model.
#'
#' @param dat_raw_dir Directory for raw existing forecasts
#' @param dat_prcsd_dir Directory for processed existing forecasts
#' @param model_equations BIMETS equation bundle (used for the exogenous list).
#' @param data_model_xts xts object of the filtered model dataset.
#' @param save_output logical indicating if data should be saved.
#'
#' @return Data with extended pseudo-exogenous variables `data_model_ext_xts`.
#'   The function also saves `data_existing_fcst_full.csv` and `.RDS` with the
#'   full forecast that was imported from dat_prcsd_dir.
#' @keywords internal
import_existing_fcst <- function(
  dat_raw_dir,
  dat_prcsd_dir,
  model_equations,
  data_model_xts,
  save_output
) {
  # load quarterly existing forecasts
  data_existing_fcst_xts <- readr::read_tsv(here::here(
    dat_raw_dir,
    stringr::str_c("QSOL1", ".TSV")
  )) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_c("QSOL2", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::mutate(
      time = lubridate::yq(.data$DATE),
      .before = "DATE",
      .keep = "unused"
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, c("@" = "_", "OCUP%" = "OCUPP"))
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with(c("OCUPP_", "TRMS_", "PPRM_")),
        .fns = ~ .x %>% as.numeric(),
        .names = "{stringr::str_replace_all(.col, 
        c('OCUPP_' = 'OCUPPADJ_', 'TRMS_' = 'TRMSADJ_', 'PPRM_' = 'PPRMADJ_'))}"
      )
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # load annual existing forecasts
  data_existing_fcst_A_xts <- readr::read_tsv(here::here(
    dat_raw_dir,
    stringr::str_c("ASOL1", ".TSV")
  )) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_c("ASOL2", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::left_join(
      readr::read_tsv(here::here(
        dat_raw_dir,
        stringr::str_c("ASOL3", ".TSV")
      )),
      by = c("DATE")
    ) %>%
    dplyr::rename("time" = "DATE") %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .x,
        c("@" = "_", "OCUP%" = "OCUPP", "$" = "_A")
      ),
      .cols = -"time"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with(c("OCUPP_", "TRMS_", "PPRM_")),
        .fns = ~ .x %>% as.numeric(),
        .names = "{stringr::str_replace_all(.col, 
        c('OCUPP_' = 'OCUPPADJ_', 'TRMS_' = 'TRMSADJ_', 'PPRM_' = 'PPRMADJ_'))}"
      )
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # identify annual series that need to be disaggregated to quarterly
  A_only <- setdiff(
    names(data_existing_fcst_A_xts),
    names(data_existing_fcst_xts) %>%
      stringr::str_replace_all(c("$" = "_A"))
  ) %>%
    stringr::str_subset("DUM", negate = TRUE)

  # disaggregate annual series to quarterly
  A_to_Q_series <- fcutils::disagg(
    data_existing_fcst_A_xts[, A_only],
    conv_type = "uhero",
    target_freq = "quarter",
    pattern = NULL
  )

  # rename disaggregated series to match quarterly naming conventions
  colnames(A_to_Q_series) <- stringr::str_replace_all(
    colnames(A_to_Q_series),
    c("_A$" = "")
  )

  # get the names of nominal income series
  nominal_names <- A_to_Q_series %>%
    names() %>%
    stringr::str_subset("^Y") %>%
    stringr::str_subset("_R_", negate = TRUE)

  # create corresponding real income series names
  # all nominal names end with a 3-letter location code
  real_names <- stringr::str_replace(
    nominal_names,
    "_([[:alpha:]]{3}$)",
    "_R_\\1"
  )

  # combine quarterly existing forecasts with disaggregated annual nominal series
  data_existing_fcst_xts <- data_existing_fcst_xts %>%
    tsbox::ts_c(A_to_Q_series %>% tsbox::ts_pick(nominal_names))

  # construct real income series from nominal income and CPI
  data_existing_fcst_xts <- data_existing_fcst_xts %>%
    tsbox::ts_c(
      data_existing_fcst_xts %>%
        tsbox::ts_tbl() %>%
        tsbox::ts_wide() %>%
        dplyr::mutate(
          100 *
            dplyr::across(
              .cols = dplyr::all_of(nominal_names),
              .names = "{real_names}"
            ) /
            .data$CPI_HON
        ) %>%
        dplyr::select("time", dplyr::all_of(real_names)) %>%
        tsbox::ts_long() %>%
        tsbox::ts_xts()
    )

  # construct additional series required by the model
  data_existing_fcst_xts$GDP_R_RES <- data_existing_fcst_xts$GDP_R_US +
    data_existing_fcst_xts$GDP_R_JP / data_existing_fcst_xts$YXR_JP

  # construct current base-year CPI from existing CPI series
  if ("CPI_B_HON" %in% names(data_model_xts)) {
    data_existing_fcst_xts <- data_existing_fcst_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      dplyr::left_join(
        tsbox::ts_chain(
          data_model_xts %>%
            tsbox::ts_pick("CPI_B_HON") %>%
            tsbox::ts_na_omit(),
          data_existing_fcst_xts %>%
            tsbox::ts_pick("CPI_HON") %>%
            tsbox::ts_na_omit()
        ) %>%
          tsbox::ts_tbl() %>%
          dplyr::mutate(id = "CPI_B_HON") %>%
          tsbox::ts_wide(),
        by = "time"
      ) %>%
      tsbox::ts_long() %>%
      tsbox::ts_xts()
  }

  # make a copy of the existing forecast data before subsetting to exog_list
  data_existing_fcst_full_xts <- data_existing_fcst_xts

  exog_list <- model_equations$vexog %>%
    stringr::str_subset(
      "^IIS_|^SIS_|^IQ|^TREND|^CONST|^DUM|^SEASON|^QV",
      negate = TRUE
    )

  message(
    "The model relies on the following exogenous series (excluding deterministic variables): \n",
    stringr::str_flatten(exog_list, collapse = ", ")
  )

  # ensure the pseudo-exogenous series are available in data_model_xts and data_existing_fcst_xts
  missing_in_model <- setdiff(exog_list, names(data_model_xts))
  if (length(missing_in_model) > 0) {
    warning(
      "The following exogenous series are missing from history: \n",
      stringr::str_flatten(missing_in_model, collapse = ", ")
    )
  }
  missing_in_fcst <- setdiff(exog_list, names(data_existing_fcst_xts))
  if (length(missing_in_fcst) > 0) {
    warning(
      "The following exogenous series are missing from existing forecast: \n",
      stringr::str_flatten(missing_in_fcst, collapse = ", ")
    )
  }

  # extend data_model_xts with the pseudo-exogenous series
  data_model_ext_xts <- data_model_xts %>%
    fcutils::multi_chain(data_existing_fcst_xts, exog_list)

  if (isTRUE(save_output)) {
    # save the pseudo-exogenous paths alongside the main dataset
    saveRDS(
      data_model_ext_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_c("data_model_ext_xts.RDS")
      )
    )

    data_model_ext_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_c("data_model_ext_xts.csv")
        )
      )

    # save the the full existing forecast (can be used for model selection)
    saveRDS(
      data_existing_fcst_full_xts,
      file = here::here(
        dat_prcsd_dir,
        stringr::str_c("data_existing_fcst_full.RDS")
      )
    )

    data_existing_fcst_full_xts %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      readr::write_csv(
        file = here::here(
          dat_prcsd_dir,
          stringr::str_c("data_existing_fcst_full.csv")
        )
      )
  }

  data_model_xts <- data_model_ext_xts
  data_model_xts
}

data_model_xts <- import_existing_fcst(
  dat_raw_dir,
  dat_prcsd_dir,
  model_equations,
  data_model_xts,
  save_output
)
