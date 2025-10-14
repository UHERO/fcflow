# **************************
# Set addfactors
# **************************

#' Set addfactors
#'
#' @param add0_qmod_xts xts object produced by [make_qmod()] containing
#' addfactors initialized to 0.
#'
#' @return Modified xts object with addfactors.
#' @keywords internal
add_qmod <- function(add0_qmod_xts) {
  stopifnot(!missing(add0_qmod_xts))

  add_qmod_xts <- add0_qmod_xts

  # set addfactors here

  add_qmod_xts
}

add_qmod(add0_qmod_xts)
