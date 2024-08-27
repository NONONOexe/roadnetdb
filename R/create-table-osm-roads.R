#' Create a table of OpenStreetMap road
#'
#' @description
#' Create a table named `osm_roads` in the database.
#' The `osm_roads` manages OpenStreetMap road data and
#' has the following columns:
#'
#' * `osm_id`: The ID of the road in OpenStreetMap
#' * `road_type`: The type of the road
#' * `road_name`: The name of the road
#' * `layer`: The layer of the road.
#' The layer represents the height of the road.
#' * `oneway`: Whether the road is one-way or not
#' * `geom`: The geometry of the road
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @return The function invisibly returns
#' `TRUE` if the table was successfully created, `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' create_table_osm_roads(con)
#' }
create_table_osm_roads <- function(conn) {
  if (dbExistsTable(conn, "osm_roads")) {
    cli_alert_warning("The table `osm_roads` already exists")
    return(invisible(FALSE))
  }
  cli_progress_step("Creating a table: `osm_roads`")
  dbWithTransaction(conn, {
    .create_table_osm_roads(conn)
    create_index_geom(conn, "osm_roads")
  })
  return(invisible(TRUE))
}
