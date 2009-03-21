\name{map.axes}
\alias{map.axes}
\title{
  Draw Axes on Geographical Maps
}
\description{Draws a set of axes on an existing map.}
\usage{
map.axes()
}
\section{Side Effects}{
x- and y-axes are drawn for the currently displayed map.  These will
display in longitude and latitude (if no projection= has been
specified in the map() call).
}
\examples{
map("state")
map.axes()
}
\keyword{hplot}

