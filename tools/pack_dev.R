# package development script
usethis::create_from_github(
  "https://github.com/UHERO/fcflow.git",
  destdir = "/Users/peterfuleky/Documents/UHERO/UHERO_work/forecast/"
)

# install.packages("devtools")
install.packages("renv")
# install.packages("tidyverse")

renv::init()

renv::install("usethis")

usethis::use_pipe()

renv::status()
renv::snapshot()

devtools::document()
devtools::load_all()
devtools::check()

usethis::use_github_action("check-standard")
usethis::use_pkgdown_github_pages()

usethis::proj_sitrep()
usethis::git_sitrep()

usethis::use_version()
