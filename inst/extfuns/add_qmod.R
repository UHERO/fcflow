# **************************
# Set addfactors
# **************************

# fmt: skip file

#' Set addfactors
#'
#' @param add0_factors_xts xts object produced by [make_model()] containing
#' addfactors initialized to 0.
#'
#' @return Modified xts object with addfactors.
#' @keywords internal
add_factors <- function(add0_factors_xts) {
  stopifnot(!missing(add0_factors_xts))

  add_factors_xts <- add0_factors_xts

  # set addfactors here

  add_factors_xts
}

add_factors_xts <- add_factors(add0_factors_xts)
