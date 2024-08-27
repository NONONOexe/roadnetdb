#' Register OpenStreetMap road data into the database
#'
#' @description
#' Register OpenStreetMap road data into the database.
#' If the specified road data has already been registered,
#' this function update the data by specified road data as new data.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @param osm_roads A data frame of OpenStreetMap road data.
#' @return Number of rows of data updated.
#' @export
#' @examples
#' \dontrun{
#' toyota_roads <- read_osm_roads(toyota)
#' register_osm_roads(con, toyota_roads)
#' }
register_osm_roads <- function(conn, osm_roads) {
  update_row_num <- 0L
  cli_progress_step(
    "Inserting the road data into the `osm_roads`",
    msg_done = "Inserted the road data, {update_row_num} rows updated"
  )
  dbWithTransaction(conn, {
    create_table_temp_roads(conn)
    dbWriteTable(conn, "temp_roads", osm_roads,
      append = TRUE,
      field.types = c(geom = "geometry(LINESTRING, 4326)")
    )
    update_row_num <- upsert_osm_roads(conn)
    dbRemoveTable(conn, "temp_roads")
  })
  cli_progress_update()
  return(invisible(update_row_num))
}
