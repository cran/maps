\name{map.cities}
\alias{map.cities}
\title{
Add Cities to Existing Map
}
\usage{
map.cities(x = world.cities, country = "", label = NULL, minpop = 0,
maxpop = Inf, capitals = 0, cex = par("cex"), \dots)
}
\description{
  Adds city locations and (optionally) names to an existing map using a
  specified database.
}
\section{NOTE}{
  This function is available only if no projection= has been
  specified in the map() call.
}
\arguments{
\item{x}{
Name of database. See \code{\link{world.cities}} to determine the structure of
the database.
}
\item{country}{
If the string country is specified, limit the displayed cities to be
from within the specified country.
}
\item{label}{
If \code{TRUE}, label all cities.  If \code{NULL},
the cities will be labelled unless there are 20 or more.
}
\item{minpop}{
The minimum value of population below which a particular city will not
be shown.
}
\item{maxpop}{
The maximum value of population above which a particular city will not
be shown.
}
\item{capitals}{
Selection of capitals-only display. Capitals may be 1 (country capital),
2 (provincial, state, or regional capital) or 3 (local capital). See
\code{\link{world.cities}} for further information.
}
\item{cex}{
The value of cex acts to override the current value of character size
expansion.
}
\item{...}{
Further plotting parameters may be specified as for the commands
\code{\link{points}} and \code{\link{text}}.
}}
\value{
No value is returned from map.cities.
}
\section{Side Effects}{
All cities within the boundaries of the plot containing the current map
are added to the plot.  Note that it is possible that the boundaries of
the plot exceed the boundaries of the map requested, and so more cities
than were expected might be shown.
}
\details{
The database is searched for all cities matching the specified criteria
and fitting within the limits of the plot currently displayed.  The
default database is of all cities that have a population greater than
100,000 or which are capital cities of a country or island territory.
The data are from around 1980-1990.
}
\seealso{
\code{\link{world.cities}}
}
\examples{
map("world", "China")
map.cities(country = "China", capitals = 2)
}
\keyword{hplot}
