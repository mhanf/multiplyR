#' load package dependencies
#'
#' @param libname libname
#' @param pkgname pkgname
#' @importFrom shiny addResourcePath
#' @return load package dependencies
#' @export
#'

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "addonbs5-assets",
    system.file("assets", package = "addonbs5")
  )
}