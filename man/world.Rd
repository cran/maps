\name{world}
\docType{data}
\alias{world}
\alias{worldMapEnv}
\title{
Low resolution World Map
}
\usage{data(worldMapEnv)}
\description{

This world database comes from a thinned cleaned-up version of the CIA
World Data Bank II data and contains approximately 30,000 points
representing the world coastlines and national boundaries.  The full
database of nearly 2 million points is found in the mapdata package.

}
\format{
The data file is merely an assignment to a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.
}
\source{
The CIA World Data Bank II is currently (mid-2003) available from 
\url{http://www.evl.uic.edu/pape/data/WDB/}.
}
\seealso{
\code{\link{map}}
}
\examples{
map('world', fill = TRUE, col = 1:10)
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT\&T Bell Laboratories Statistics Research Report [95.2], 1995.}
}
\keyword{datasets}
