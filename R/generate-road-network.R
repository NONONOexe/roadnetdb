#' Generate road network from road data
#'
#' @description
#' The function generates a road network in the specified area.
#' A road network has intersection as nodes and road segments as edges.
#' The function creates two tables representing edges and nodes of the network.
#'
#' The function generates a road network by the following steps:
#'
#' 1. Generate road network edges from road data
#' 2. Generate road network nodes from road network edges
#' 3. Store the road network edges and nodes into the database
#'
#' The road network edge table has the following columns:
#'
#' * `id`: The ID of the edge
#' * `source`: The ID of the source node
#' * `target`: The ID of the target node
#' * `oneway`: Whether the edge is one-way or not
#' * `length`: The length of the edge in meters
#' * `parent`: The ID of the original road in OpenStreetMap
#' * `geom`: The geometry of the edge
#'
#' The road network nodes table has the following columns:
#'
#' * `id`: The ID of the node
#' * `geom`: The geometry of the node
#'
#' The nodes are computed by `pgr_createTopology` function in pgRouting.
#' If the distance between two nodes is less than 1 meter
#' in Web Mercator (EPSG 3857), the two nodes are merged into one node.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @param area A `sf`, area to be computed.
#' @param edge_table A `character`, the name of the road network edge table
#' (default: `road_edges`).
#' @param node_table A `character`, the name of the road network node table
#' (default: `road_nodes`).
#' @return Number of rows of data updated.
#' @export
#' @examples
#' \dontrun{
#' generate_road_network(con, toyota)
#' }
generate_road_network <- function(
    conn, area, node_table = "road_nodes", edge_table = "road_edges") {
  # Check if the target tables not exist
  if (dbExistsTable(conn, edge_table)) {
    cli_alert_warning("The table `{edge_table}` already exists")
    return(invisible(0L))
  }
  if (dbExistsTable(conn, node_table)) {
    cli_alert_warning("The table `{node_table}` already exists")
    return(invisible(0L))
  }

  dbWithTransaction(conn, {
    cli_progress_step("Generating the edges")
    create_table_temp_area(conn)
    write_table_temp_area(conn, area)
    create_table_temp_edges(conn)
    generate_road_edges(conn)
    drop_table_temp_area(conn)

    cli_progress_step("Generating the nodes")
    create_topology(conn)

    node_count <- 0L
    edge_count <- 0L
    cli_progress_step(
      "Registering the road network into the database",
      msg_done = "Registered the road network, {node_count} nodes and {edge_count} edges"
    )
    node_count <- register_road_nodes(conn, node_table)
    edge_count <- register_road_edges(conn, edge_table)
    cli_progress_update()

    # Clean up
    dbRemoveTable(conn, glue("temp_edges"))
    dbRemoveTable(conn, glue("temp_edges_vertices_pgr"))
  })
  cli_progress_update()
  return(invisible(edge_count + node_count))
}

register_road_nodes <- function(conn, node_table_name) {
  create_table_nodes(conn, node_table_name)
  create_index_geom(conn, node_table_name)
  insert_nodes(conn, node_table_name)
}

register_road_edges <- function(conn, edge_table_name) {
  create_table_edges(conn, edge_table_name, "osm_roads", "osm_id")
  create_index_geom(conn, edge_table_name)
  insert_edges(conn, edge_table_name)
}
