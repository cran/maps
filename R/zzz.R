.First.lib <- function(lib, pkg) {
  if(!exists("Sys.setenv", envir = baseenv()))
    Sys.setenv <- Sys.putenv
  if (Sys.getenv("R_MAP_DATA_DIR") == "")
    Sys.setenv("R_MAP_DATA_DIR"=paste(lib, pkg, "mapdata/", sep="/"))
  library.dynam("maps", pkg, lib)
}
