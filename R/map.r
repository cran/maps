# R data structure for maps is list(x, y, names)
# names is character vector naming the polygons
# (contiguous groups of non-NA coordinates)

# returns the jth contiguous section of non-NA values in vector x
# should consecutive NA's count as one?
subgroup <- function(x, i) {
  n <- length(x)
  breaks <- (1:n)[is.na(x)]
  if(length(breaks) == 0) {
    starts <- 1; ends <- n
  } else {
    starts <- c(1, breaks + 1)
    ends <- c(breaks - 1, n)
  }
  result <- numeric(0)
  for(j in i) {
    p <- x[starts[j]:ends[j]]
    if(length(result) == 0) result <- p
    else result <- c(result, NA, p)
  }
  result
}

sub.polygon <- function(p, i) {
  lapply(p[c("x", "y")], function(x) subgroup(x, i))
}

# returns a sub-map of the named map corresponding to the given regions
# regions is a vector of regular expressions to match to the names in the map
# regions outside of xlim, ylim may be omitted
map.poly <- function(database, regions = ".", exact = FALSE,
                     xlim = NULL, ylim = NULL,
                     boundary = TRUE, interior = TRUE, as.polygon = FALSE) {
  if(!is.character(database)) {
    if(!as.polygon) stop("map objects require as.polygon=TRUE")
    the.map <- database
    if(regions == ".") {
      # speed up the common case
      nam = the.map$names
      coord <- the.map[c("x", "y")]
    } else {
      regions <- tolower(regions)
      # bug: doesn't implement exact
      regexp <- paste("(^", regions, ")", sep = "", collapse = "|")
      r <- grep(regexp, the.map$names)
      if(length(r) == 0) stop("no recognized region names")
      nam <- the.map$names[r]
      coord <- sub.polygon(the.map, r)
    }
    coord$range <- c(range(coord$x, na.rm = TRUE), range(coord$y, na.rm = TRUE))
  } else {
    # turn the polygon numbers into a list of polyline numbers
    gon <- mapname(database, regions, exact)
    n <- length(gon)
    if(n == 0) stop("no recognized region names")
    if(is.null(xlim)) xlim <- c(-1e+30, 1e+30)
    if(is.null(ylim)) ylim <- c(-1e+30, 1e+30)
    # turn the polygon numbers into a list of polyline numbers
    line <- mapgetg(database, gon, as.polygon, xlim, ylim)
    if(length(line$number) == 0)
            stop("nothing to draw: all regions out of bounds")
    # turn the polyline numbers into x and y coordinates
    if(as.polygon) {
      coord <- mapgetl(database, line$number, xlim, ylim) 
      # assemble lines into polygons
      gonsize <- line$size
      keep <- rep(TRUE, length(gonsize))
      coord[c("x", "y")] <- makepoly(coord, gonsize, keep)
    }
    else {
      l <- abs(line$number)
      if(boundary && interior) l <- unique(l)
      else if(boundary) l <- l[!match(l, l[duplicated(l)], FALSE)]
      else l <- l[duplicated(l)]
      coord <- mapgetl(database, l, xlim, ylim)
      if(length(coord) == 0)
              stop("all data out of bounds")
    }
    nam <- line$name
  }
  list(x = coord$x, y = coord$y, range = coord$range, names = nam)
}

map <-
function(database = "world", regions = ".", exact = FALSE, boundary = TRUE, 
         interior = TRUE, projection = "", parameters = NULL, 
         orientation = NULL, fill = FALSE, color = 1,
         plot = TRUE, add = FALSE, namesonly = FALSE, 
         xlim = NULL, ylim = NULL, wrap = FALSE, resolution = 1, type = "l",
         bg = par("bg"), mar = c(0, 0, par("mar")[3], 0.1), ...)
{
  # parameter checks
  if(!missing(resolution) && !plot) 
    stop("must have plot=TRUE if resolution is given")
  if(!fill && !boundary && !interior)
    stop("one of boundary and interior must be TRUE")
  doproj <- !missing(projection) || !missing(parameters) || !missing(
          orientation)
  coordtype <- maptype(database)
  if (coordtype == "unknown") 
     stop("missing database or unknown coordinate type")
  if (doproj && coordtype != "spherical") 
    stop(paste(database, "database is not spherical; projections not allowed"))
  # turn the region names into x and y coordinates
  if(is.character(database)) as.polygon = fill
  else as.polygon = TRUE
  coord <- map.poly(database, regions, exact, xlim, ylim, 
                    boundary, interior, as.polygon)
  if(is.na(coord$x[1])) stop("first coordinate is NA.  bad map data?")
  if(plot) {
    assign(".map.range", coord$range, envir = globalenv())
  }
  if(doproj) {
    nam <- coord$names
    library(mapproj)
    coord <- mapproject(coord, pr = projection, pa = parameters,
                        or = orientation)
    if(plot && coord$error)
      if(all(is.na(coord$x)))
        stop("projection failed for all data")
      else warning("projection failed for some data")
    coord$names <- nam
  }
  # do the plotting, if requested
  if(plot) {
    # for new plots, set up the coordinate system;
    # if a projection was done, set the aspect ratio
    # to 1, else set it so that a long-lat square appears
    # square in the middle of the plot
    if(!add) {
      # this doesn't work until R's par() mechanism is fixed
      #par(mar = mar, xpd = TRUE)
      opar = par(bg = bg)
      if(!par("new")) plot.new()
      # xlim, ylim apply before projection
      if(is.null(xlim) || doproj) xrange <- range(coord$x, na.rm = TRUE)
      else xrange <- xlim
      if(is.null(ylim) || doproj) yrange <- range(coord$y, na.rm = TRUE)
      else yrange <- ylim
      border <- c(0.01, 0.01)
      if(coordtype != "spherical" || doproj) aspect <- c(1, 1) 
      else aspect <- c(cos((mean(yrange) * pi)/180), 1)
      d <- c(diff(xrange), diff(yrange)) * aspect
      if(TRUE) {
        plot.window(xrange, yrange, asp = aspect[2]/aspect[1])
        #set.aspect(aspect[2]/aspect[1])
        #layout(1, width = aspect[1], height = aspect[2], respect = TRUE)
        #on.exit(layout(1), add = TRUE)
      } else if(TRUE) {
        # must have par(xpd = FALSE) for limits to have an effect
        par(pin = par("fin"))
        p <- par("pin")
        p <- d*min(p/d)
        par(pin = p)
        d <- d*border + ((p/min(p/d) - d)/2)/aspect
        usr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, c(2, 2))
        par(usr = usr)
      } else {
        par(pin = par("fin"))
        aspect <- d[2]/d[1]
        fin <- par("fin")
        a <- aspect*fin[1]/fin[2]
        if(a > 1) plt <- c(0.5 - 1/a/2, 0.5 + 1/a/2, 0, 1)
        else plt <- c(0, 1, 0.5 - a/2, 0.5 + a/2)
        d <- c(diff(xrange), diff(yrange))*0.02
        usr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, c(2, 2))
        par(usr = usr)
        opar <- c(opar, par(plt = plt))
      }
      on.exit(par(opar))
      #par(usr = usr)
    }
    # thinning only works if you have polylines from a database
    if(is.character(database) && resolution != 0 && type != "n") {
      pin <- par("pin")
      usr <- par("usr")
      resolution <- resolution * min(diff(usr)[-2]/pin/100)
      coord[c("x", "y")] <- mapthin(coord, resolution)
    }
    if(type != "n") {
      # suppress warnings about clipping
      #oerr <- par(err = -1)
      #on.exit(par(oerr))
      if(wrap) coord = map.wrap(coord)
      if (fill) polygon(coord, col = color, ...)
      else lines(coord, col = color, type = type, ...)
      #if(fill) polygon(coord, col = color, border = NA, ...)
      #else polygon(coord, col = NA, border = color, ...)
    }
  }
  # return value is names or coords, but not both
  class(coord) = "map"
  value <- if(namesonly) coord$names else coord
  if(plot) invisible(value)
  else value
}

"makepoly" <-
function(xy, gonsize, keep)
{
  # remove NAs and duplicate points so that a set of polylines becomes a set
  # of polygons.
  # xy is a set of polylines, separated by NAs.
  # gonsize is a vector, giving the number of lines to put into each polygon
  # note that a polyline may consist of a single point
  x <- xy$x
  y <- xy$y
  n <- length(x)
  gonsize <- gonsize[ - length(gonsize)]
  discard <- seq(length(x))[is.na(x)]
  if(length(discard) > 0) {
    # locations of (possible) duplicate points
    dups = c(discard - 1, n)
    # only polylines with > 1 point have duplicates
    i = which(diff(c(0, dups)) > 2);
    discard <- c(discard, dups[i]);
  }
  # first part of discard is the NAs, second part is duplicates
  # gonsize tells us which NAs to preserve
  if(length(gonsize) > 0)
    discard <- discard[ - cumsum(gonsize)]
  if(length(discard) > 0) {
    x <- x[ - discard]
    y <- y[ - discard]
  }
  keep <- rep(keep, diff(c(0, seq(length(x))[is.na(x)], length(x))))
  closed.polygon(list(x = x[keep], y = y[keep]))
}
closed.polygon <- function(p) {
  # p is a set of polylines, separated by NAs
  # for each one, the first point is copied to the end, giving a closed polygon
  x = p$x
  y = p$y
  n = length(x)
  breaks <- seq(length(x))[is.na(x)]
  starts <- c(1, breaks + 1)
  ends <- c(breaks - 1, n)
  x[ends + 1] = x[starts]
  y[ends + 1] = y[starts]
  x = insert(x, breaks + 1)
  y = insert(y, breaks + 1)
  list(x = x, y = y)
}
insert <- function(x, i, v = NA) {
  # insert v into an array x, at positions i
  # e.g. insert(1:7, c(2, 5, 8))
  n = length(x)
  new.n = n + length(i)
  m = logical(new.n)
  i.new = i - 1 + seq(length(i))
  m[i.new] = TRUE
  x = x[(1:new.n) - cumsum(m)]
  x[i.new] = v
  x
}
