"map.old" <-
function (database = "state", regions = ".", exact = F, boundary = T, 
    interior = T, fill = F, projection = "", parameters = NULL, 
    orientation = rep(NA, 3), color = 1, add = F, plot = T, namesonly = F, 
    xlim = c(-1e+30, 1e+30), ylim = c(-1e+30, 1e+30), resolution = 1, 
    type = "l", ...) 
{
  "makepoly" <- function(xy, gonsize, keep) {
    x <- xy$x
    y <- xy$y
    n <- length(x)
    gonsize <- gonsize[ - length(gonsize)]
    discard <- seq(x)[is.na(x)]
    if(length(discard) > 0)
      discard <- c(discard, discard - 1, n)
    if(length(gonsize) > 0)
      discard <- discard[ - cumsum(gonsize)]
    if(length(discard) > 0) {
      x <- x[ - discard]
      y <- y[ - discard]
    }
    keep <- rep(keep, diff(c(0, seq(x)[is.na(x)], length(x))))
    list(x = x[keep], y = y[keep])
  }

    if (!missing(resolution) && !plot) 
        stop("must have plot=T if resolution is given")
    if (!fill && !boundary && !interior) 
        stop("one of boundary and interior must be TRUE")
    doproj <- !missing(projection) || !missing(parameters) || 
        !missing(orientation)
    coordtype <- maptype(database)
    if (coordtype == "unknown") 
        stop("missing database or unknown coordinate type")
    if (doproj && coordtype != "spherical") 
        stop(paste(database, "database is not spherical; projections not allowed"))
    gon <- mapname(database, regions, exact)
    n <- length(gon)
    if (n == 0) 
        stop("nothing to draw: no recognized region names")
    line <- mapgetg(database, gon, fill, xlim, ylim)
    if (length(line$number) == 0) {
        if (missing(xlim) || missing(ylim)) 
            stop("nothing to draw: all regions out of bounds")
        else coord <- list(x = c(xlim[1], NA, xlim[2]), y = c(ylim[1], 
            NA, ylim[2]), range = c(xlim, ylim))
        if (fill) 
            stop("cannot fill: all data out of bounds")
    }
    else {
        if (fill) 
            coord <- mapgetl(database, line$number, xlim, ylim)
        else {
            l <- abs(line$number)
            if (boundary && interior) 
                l <- unique(l)
            else if (boundary) 
                l <- l[!match(l, l[duplicated(l)], F)]
            else l <- l[duplicated(l)]
            coord <- mapgetl(database, l, xlim, ylim)
            if (length(coord) == 0) 
                if (missing(xlim) || missing(ylim)) 
                  stop("all data out of bounds")
                else coord <- list(x = c(xlim[1], NA, xlim[2]), 
                  y = c(ylim[1], NA, ylim[2]), range = c(xlim, 
                    ylim))
        }
    }
    if (doproj) {
        coord <- mapproject(coord, pr = projection, pa = parameters, 
            or = orientation)
        if (plot && coord$error) 
            if (all(is.na(coord$x))) 
                stop("projection failed for all data")
            else warning("projection failed for some data")
    }
    if (fill) {
        gonsize <- line$size
        color <- rep(color, length = length(gonsize))
        keep <- !is.na(color)
        coord[c("x", "y")] <- makepoly(coord, gonsize, keep)
        color <- color[keep]
    }
    if (plot) {
        if (!add) {
	    # remove margins
            par(pin = par("fin"))
            plot.new()
            #xrange <- coord$range[1:2]
            #yrange <- coord$range[3:4]
            xrange <- range(coord$x, na.rm = T)
            yrange <- range(coord$y, na.rm = T)
	    border <- c(0.01, 0.01)
            if (!missing(xlim)) {
                xrange[1] <- max(xrange[1], xlim[1])
                xrange[2] <- min(xrange[2], xlim[2])
		border[1] <- 0
            }
            if (!missing(ylim)) {
                yrange[1] <- max(yrange[1], ylim[1])
                yrange[2] <- min(yrange[2], ylim[2])
		border[2] <- 0
            }
            aspect <- if (coordtype != "spherical" || doproj) 
                c(1, 1)
            else c(cos((mean(yrange) * pi)/180), 1)
            d <- c(diff(xrange), diff(yrange)) * aspect
            p <- par("pin")
            # assumes coordinates are already clipped
            p <- d*min(p/d)
	    par(pin = p)
            d <- d*border + ((p/min(p/d) - d)/2)/aspect
            usr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, c(2, 2))
            par(usr = usr)
        }
        if (resolution != 0 && type != "n") {
            pin <- par("pin")
            usr <- par("usr")
            resolution <- resolution * min(diff(usr)[-2]/pin/100)
	    coord[c("x", "y")] <- mapthin(coord, resolution)
        }
        if (type != "n") {
            #oerr <- par(err = -1)
            #on.exit(par(oerr))
            if (fill) 
                polygon(coord, col = color, ...)
            else lines(coord, col = color, type = type, ...)
        }
    }
    value <- if (namesonly) 
        line$name
    else coord[c("x", "y", "range")]
    if (plot) 
        invisible(value)
    else value
}
