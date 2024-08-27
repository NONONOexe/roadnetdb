#' Enable database extensions.
#'
#' @description
#' Enable database extensions.
#'
#' @param conn A `DBIConnection`, a connection object to the database.
#' @return The function returns
#' `FALSE` if enabling fails or `TRUE` if enabling succeeds.
#' @name enable_extension
#' @aliases NULL
#' @examples
#' \dontrun{
#' enable_postgis(con)
#'
#' enable_pgrouting(con)
#' }
NULL

enable_extension <- function(conn, extension_name) {
  sql <- glue_sql("CREATE EXTENSION {`extension_name`}", .con = conn)
  dbExecute(conn, sql)
  cli_alert_success("The extension is enabled: {extension_name}")
  return(invisible(TRUE))
}

#' @export
#' @rdname enable_extension
enable_postgis <- function(conn) {
  enable_extension(conn, "postgis")
}


#' @export
#' @rdname enable_extension
enable_pgrouting <- function(conn) {
  enable_extension(conn, "pgrouting")
}
