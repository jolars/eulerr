.eulerr_env <- new.env(parent = emptyenv())
assign("options", list(), envir = .eulerr_env)

.onLoad <- function(libname, pkgname)
  eulerr_options(eulerr_default_options())

.onUnload <- function(libpath)
  library.dynam.unload("eulerr", libpath)
