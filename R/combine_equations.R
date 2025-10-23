# *************************
# Combine individual equation files into a model definition
# *************************

#' Load Lines from an Individual Equation File
#'
#' @param eq_file Path to an individual equation file.
#' @return Tibble containing the BIMETS-relevant lines from the equation file.
#'
#' @keywords internal
#' @noRd
read_eq <- function(eq_file) {
  readr::read_lines(eq_file) %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(
      .data$value,
      c(
        "(?<!#)IDENTITY>|(?<!#)BEHAVIORAL>|(?<!#)TSRANGE|(?<!#)EQ>|(?<!#)COEFF>"
      )
    )) %>%
    dplyr::add_row(value = "", .before = 1) %>%
    dplyr::add_row(value = "")
}


#' Combine Individual Equation Files
#'
#' Reads individual equation text files, filters by optional subset, and
#' writes a combined BIMETS model file for the current vintage. The individual
#' equation files should be stored at the equations path specified in the
#' configuration file and their names have to end with "eq.txt".
#'
#' @param cfg Configuration list from [load_forecast_cfg()].
#' @return Invisible BIMETS model object created from the combined equations
#' @export
combine_equations <- function(cfg = load_forecast_cfg()) {
  equations_dir <- require_cfg(cfg, c("paths", "equations"))
  equations_subset <- require_cfg(
    cfg,
    c("combine_equations", "equations_subset")
  )
  equations_file <- require_cfg(cfg, c("combine_equations", "equations_file"))
  save_output <- require_cfg(cfg, c("combine_equations", "save_output"))

  # list all variable specific files in the equations directory
  list_of_files <- fs::dir_ls(path = here::here(equations_dir)) %>%
    stringr::str_subset("eq.txt")

  # filter the list of files to those matching the subset of variables names
  if (!is.null(equations_subset) && length(equations_subset) > 0) {
    list_of_files <- list_of_files %>%
      stringr::str_subset(stringr::str_flatten(
        equations_subset,
        collapse = "|"
      ))
  }

  # combine the snippets into one cohesive BIMETS model definition
  all_eqs <- list_of_files %>%
    purrr::map(read_eq) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::filter(stringr::str_detect(.data$value, "^#", negate = TRUE)) %>%
    dplyr::add_row(value = "MODEL\n", .before = 1) %>%
    dplyr::add_row(value = "\nEND")

  # write the combined text so it can be inspected or reloaded later
  if (isTRUE(save_output)) {
    readr::write_lines(
      all_eqs %>% dplyr::pull(.data$value),
      here::here(equations_dir, equations_file)
    )
  }

  # load the combined text file into a BIMETS model object the rest of the workflow can use
  model_equations <- bimets::LOAD_MODEL(
    modelFile = here::here(equations_dir, equations_file)
  )

  # save the BIMETS object so estimation/solving can reuse it without re-parsing text
  if (isTRUE(save_output)) {
    saveRDS(
      model_equations,
      file = here::here(
        equations_dir,
        equations_file %>% stringr::str_replace(".txt$", ".RDS")
      )
    )
  }

  invisible(
    list(
      model_equations = model_equations
    )
  )
}

if (identical(environment(), globalenv())) {
  combine_equations()
}

# **************************
# end ----
# **************************
