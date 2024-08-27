#' Download road data from OpenStreetMap
#'
#' @description
#' The function download road data from OpenStreetMap using overpass query.
#' Roads are records have the following values for the key, 'highway'
#' in OpenStreetMap data.
#'
#'   * motorway
#'   * trunk
#'   * primary
#'   * secondary
#'   * tertiary
#'   * unclassified
#'   * residential
#'   * motorway_link
#'   * trunk_link
#'   * primary_link
#'   * secondary_link
#'   * tertiary_link
#'
#' For one-way roads, the column `oneway` is `TRUE`.
#'
#' @param area A `sf`, area to be downloaded
#' @return OpenStreetMap road data
#' @export
#' @examples
#' \dontrun{
#' download_osm_roads(toyota)
#' }
download_osm_roads <- function(area) {
  row_num <- 0L
  cli_progress_step(
    "Downloading road data from OpenStreetMap",
    msg_done = "Downloaded {row_num} roads"
  )

  downloaded_data <- area |>
    st_bbox() |>
    opq(timeout = 180) |>
    add_osm_feature(
      key = "highway",
      value = c(
        "motorway", "trunk", "primary", "secondary",
        "tertiary", "unclassified", "residential",
        "motorway_link", "trunk_link", "primary_link",
        "secondary_link", "tertiary_link"
      )
    ) |>
    osmdata_sf() |>
    pluck("osm_lines") |>
    st_filter(area)

  if ("oneway" %in% names(downloaded_data)) {
    directed_roads <- downloaded_data |>
      mutate(
        geometry = if_else(!is.na(.data$oneway) & .data$oneway == "-1",
                           st_reverse(.data$geometry), .data$geometry),
        oneway = !is.na(.data$oneway) & .data$oneway %in% c("-1", "yes")
      )
  } else {
    directed_roads <- mutate(downloaded_data, oneway = FALSE)
  }

  osm_roads <- directed_roads |>
    transmute(
      osm_id = as.integer(.data$osm_id),
      road_type = .data$highway,
      road_name = .data$name,
      layer = replace_na(as.integer(.data$layer), 0),
      oneway = .data$oneway
    ) |>
    rename("geom" = "geometry") |>
    st_cast("LINESTRING")

  row_num <- nrow(osm_roads)

  cli_progress_update()
  return(osm_roads)
}
