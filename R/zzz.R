.onAttach <- function(libname, pkgname) {
}


.onLoad <- function(libname, pkgname) {
}


.onUnload <- function(libpath) {
  library.dynam.unload("GCAMCTS", libpath)
}
