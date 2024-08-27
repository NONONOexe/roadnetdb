#' Fetch OpenStreetMap road data from the database
#'
#' @description
#' The function fetch road data intersecting with the specified area
#' from `osm_roads` table in the database.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @param area A `sf`, area to be fetched.
#' @return OpenStreetMap road data.
#' @format A sf with 6 variables:
#' \describe{
#' \item{osm_id}{The ID of the road in OpenStreetMap}
#' \item{road_type}{The type of the road}
#' \item{road_name}{The name of the road}
#' \item{layer}{The layer of the road. The layer represents the height of the road.}
#' \item{oneway}{Whether the road is one-way or not}
#' \item{geom}{The geometry of the road}
#' }
#' @export
#' @examples
#' \dontrun{
#' fetch_osm_roads(con, toyota)
#' }
fetch_osm_roads <- function(conn, area) {
  osm_roads <- NULL
  row_num <- 0L
  cli_progress_step("Fetching data from the `osm_roads`",
                    msg_done = "Fetched {row_num} roads}")
  dbWithTransaction(conn, {
    create_table_temp_area(conn)
    write_table_temp_area(conn, area)
    osm_roads <- select_osm_roads(conn) |>
      mutate(geom = st_as_sfc(.data$geom)) |>
      st_sf()
    drop_table_temp_area(conn)
  })
  row_num <- nrow(osm_roads)
  cli_progress_update()
  return(osm_roads)
}
