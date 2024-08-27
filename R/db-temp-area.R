create_table_temp_area <- function(conn) {
  sql <- "
      CREATE TEMPORARY TABLE temp_area (
        geom geometry(MULTIPOLYGON, 4326) NOT NULL
      )
    "
  dbExecute(conn, sql)
}

write_table_temp_area <- function(conn, area) {
  merged <- st_sf(geom = st_cast(st_union(area), "MULTIPOLYGON"))
  dbWriteTable(conn, "temp_area", merged,
               append = TRUE,
               field.types = c(geom = "geometry(MULTIPOLYGON, 4326)")
  )
}

drop_table_temp_area <- function(conn) {
  dbRemoveTable(conn, "temp_area")
}
