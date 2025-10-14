# **************************
# Run user script ----
# **************************

#' Run a user-supplied script with arguments
#'
#' @description
#' This function sources and executes an R script from a specified file path
#' in a new environment, optionally passing objects to that environment.
#'
#' @param path A character string specifying the path to the R script to be run.
#' @param ... Additional named arguments to be passed to the script's environment.
#' @return The function invisibly returns the environment in which the script was evaluated.
#' @export
#' @examples
#' # Create a sample data frame
#' my_data <- data.frame(a = 1:5, b = letters[1:5])
#'
#' # Create a temporary script for demonstration
#' temp_script <- tempfile(fileext = ".R")
#' writeLines(
#'   "print('Executing script with data...')\nprint(head(data))",
#'   temp_script
#' )
#'
#' # Run the script, passing the data frame
#' run_script_with_args(temp_script, data = my_data)
#'
#' # Clean up
#' unlink(temp_script)
run_script_with_args <- function(path, ...) {
  if (!file.exists(path)) {
    stop("File not found at specified path: ", path)
  }

  message("Running script: ", path)

  # Create a new environment for the script
  script_env <- new.env()

  # Place all named arguments into the new environment
  args <- list(...)
  for (name in names(args)) {
    assign(name, args[[name]], envir = script_env)
  }

  # Use sys.source to run the script in the new environment
  sys.source(path, envir = script_env)

  invisible(script_env)
}

# **************************
# end ----
# **************************
