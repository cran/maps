.First.lib <- function(lib, pkg) {
  # minka: only do this if R_MAP_DATA_DIR doesn't exist?
  if (Sys.getenv("R_MAP_DATA_DIR") == "")
    Sys.putenv("R_MAP_DATA_DIR"=paste(lib, pkg, "mapdata/", sep="/"))
  library.dynam("maps", pkg, lib)
}
