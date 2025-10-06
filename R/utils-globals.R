# shut up the dot
utils::globalVariables(c(".", ".data"))

.onLoad <- function(libname, pkgname) {
  # set the bimets version in options
  options(
    "BIMETS_VERSION" = utils::packageVersion("bimets")
  )
}
