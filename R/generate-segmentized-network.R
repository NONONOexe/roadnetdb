#' Generate segmentized network from road network edges
#'
#' @description
#' The function generates a segmentized network in the specified area.
#' A segmentized network is a network in which the edges of a road network
#' are divided into segments of a specified length.
#' The function creates two tables representing edges and nodes of the network.
#'
#' The function generates a segmentized network by the following steps:
#'
#' 1. Split road network edges into segments as segmentized network edges
#' 2. Generate segmentized network nodes from segmentized network edges
#' 3. Store the segmentized network edges and nodes into the database
#'
#' The node and edge tables of the segmentized network have the same columns
#' as the road network node and edge tables.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @param source_table A `character`, the name of the road network edge table
#' (default: `road_edges`).
#' @param edge_table A `character`, the name of the segmentized network
#' edge table (default: `segment_edges`).
#' @param node_table A `character`, the name of the segmentized network
#' node table (default: `segment_nodes`).
#' @param segment_length A `numeric`, the length of segments in meters
#' (default: 10).
#' @return Number of rows of data updated.
#' @seealso \code{\link{generate_road_network}}
#' @export
#' @examples
#' \dontrun{
#' generate_segmentized_network(con)
#' }
generate_segmentized_network <- function(
    conn, source_table = "road_edges",
    node_table = "segment_nodes", edge_table = "segment_edges",
    segment_length = 10) {
  # Check if the source table exists
  if (!dbExistsTable(conn, source_table)) {
    cli_alert_warning("The table `{source_table}` does not exist")
    return(invisible(0L))
  }

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
    create_table_temp_edges(conn)
    generate_segment_edges(conn, source_table, segment_length)

    cli_progress_step("Generating the nodes")
    create_topology(conn)

    edge_count <- 0L
    node_count <- 0L
    cli_progress_step(
      "Registering the segmentized network into the database",
      msg_done = "Registered the segmentized network, {node_count} nodes and {edge_count} edges"
    )
    node_count <- register_segment_nodes(conn, node_table)
    edge_count <- register_segment_edges(conn, edge_table, source_table)
    cli_progress_update()

    # Clean up
    dbRemoveTable(conn, glue("temp_edges"))
    dbRemoveTable(conn, glue("temp_edges_vertices_pgr"))
  })
  return(invisible(edge_count + node_count))
}

register_segment_nodes <- function(conn, node_table) {
  create_table_nodes(conn, node_table)
  create_index_geom(conn, node_table)
  insert_nodes(conn, node_table)
}

register_segment_edges <- function(conn, edge_table, source_table) {
  create_table_edges(conn, edge_table, source_table, "id")
  create_index_geom(conn, edge_table)
  insert_edges(conn, edge_table)
}
