# **************************
# Extend Master Dataset
# **************************

#' Optionally Extend Master Dataset with AREMOS History
#'
#' Pulls 11Q4 AREMOS exports and splices longer histories onto the main
#' dataset when requested.
#'
#' @param data_main_xts xts object containing the UDAMAN pull.
#' @param dat_raw_dir Base directory for raw data files.
#'
#' @return xts object, potentially extended with archived data.
#' @keywords internal
extend_qmain_history <- function(data_main_xts, dat_raw_dir) {
  # load aremos data from the 11Q4 subfolder
  data_aremos_hist_xts <- c(
    "TOUR1",
    "TOUR2",
    "TOUR3",
    "JP1",
    "US1",
    "TAX1",
    "TAX2",
    "BEA1",
    "BLS1",
    "BLS2",
    "BLS3",
    "MISC1",
    "MISC2"
  ) %>%
    purrr::map(
      ~ readr::read_tsv(here::here(
        dat_raw_dir,
        "11Q4",
        stringr::str_glue(.x, ".TSV")
      ))
    ) %>%
    purrr::reduce(~ dplyr::full_join(.x, .y, by = c("DATE"))) %>%
    dplyr::mutate(
      time = lubridate::yq(.data$DATE),
      .before = "DATE",
      .keep = "unused"
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(., c("@" = "_", "OCUP%" = "OCUPP"))
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .,
        c("OCUPP" = "OCCUPPADJ", "TRMS" = "TRMSADJ")
      )
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  # identify series in data_main_xts have a longer history in data_aremos_hist_xts
  ser_ext <- dplyr::full_join(
    tsbox::ts_summary(data_main_xts),
    tsbox::ts_summary(data_aremos_hist_xts),
    by = "id"
  ) %>%
    dplyr::filter(.data$start.x > .data$start.y)

  message(
    "The history of the following series will be extended: ",
    stringr::str_flatten(ser_ext$id, collapse = ", ")
  )

  # extend data_main_xts with the longer histories from aremos
  data_main_xts <- data_main_xts %>%
    fcutils::multi_chain(data_aremos_hist_xts, ser_ext$id)

  data_main_xts
}

data_main_xts <- extend_qmain_history(data_main_xts, dat_raw_dir)
