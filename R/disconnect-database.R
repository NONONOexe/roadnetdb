#' Disconnect from the database
#'
#' @description
#' Disconnect from the database.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @return The function returns
#' `TRUE` if this is connected and disconnecting was done as expected or
#' `FALSE` if this is not connected to the database.
#' @export
#' @examples
#' \dontrun{
#' disconnect_database(con)
#' }
disconnect_database <- function(conn) {
  cli_progress_step("Disconnecting from the database",
                    msg_done = "Disconnected from the database")
  dbDisconnect(conn)
}
