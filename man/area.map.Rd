\author{Tom Minka}
\name{area.map}
\alias{area.map}
\title{
  Area of map regions
}
\usage{
area.map(m, regions = ".", ...)
}
\description{Computes the areas of regions in a map.}
\arguments{
  \item{m}{a map object containing named polygons (created with
  \code{fill = TRUE}).}
  \item{regions}{a character vector naming one of more regions, as in
    \code{\link{map}}.}
  \item{...}{additional arguments to \code{\link{match.map}}}
}
\details{
  The area of each matching region in the map is computed, and
  regions which match the same element of \code{regions} have their
  areas combined.
}
\value{
  a named vector of region areas.
}
\seealso{\code{area.polygon}, \code{apply.polygon}}
\examples{
m = map("state", fill = TRUE)
area.map(m)
area.map(m, ".*dakota")
area.map(m, c("North Dakota", "South Dakota"))
}
\keyword{dplot}
