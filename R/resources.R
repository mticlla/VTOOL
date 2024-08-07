#' @noRd
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = fs::path_package(
      "app/www",
      package = "VTOOL"
    )
  )
}

#' @noRd
.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
}
