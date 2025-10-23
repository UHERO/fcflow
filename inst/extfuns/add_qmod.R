# **************************
# Set addfactors
# **************************

#' Set addfactors
#'
#' @param addfactors0_xts xts object produced by [make_model()] containing
#' addfactors initialized to 0.
#'
#' @return Modified xts object with addfactors.
#' @keywords internal
add_qmod <- function(addfactors0_xts) {
  stopifnot(!missing(addfactors0_xts))

  addfactors_xts <- addfactors0_xts

  # set addfactors here

  addfactors_xts
}

addfactors_xts <- add_qmod(addfactors0_xts)
