% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch-osm-roads.R
\name{fetch_osm_roads}
\alias{fetch_osm_roads}
\title{Fetch OpenStreetMap road data from the database}
\format{
A sf with 6 variables:
\describe{
\item{osm_id}{The ID of the road in OpenStreetMap}
\item{road_type}{The type of the road}
\item{road_name}{The name of the road}
\item{layer}{The layer of the road. The layer represents the height of the road.}
\item{oneway}{Whether the road is one-way or not}
\item{geom}{The geometry of the road}
}
}
\usage{
fetch_osm_roads(conn, area)
}
\arguments{
\item{conn}{A \code{DBIConnection}, a connection object to the database.}

\item{area}{A \code{sf}, area to be fetched.}
}
\value{
OpenStreetMap road data.
}
\description{
The function fetch road data intersecting with the specified area
from \code{osm_roads} table in the database.
}
\examples{
\dontrun{
fetch_osm_roads(con, toyota)
}
}
