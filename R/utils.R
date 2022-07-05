# This is a common place to define small utilities that are used inside multiple package functions. 
# Since they serve as helpers to multiple functions, placing them in R/utils.R makes them easier to re-discover 
# when you return to your package after a long break.

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package")
}

onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Your name goes here",
    devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}