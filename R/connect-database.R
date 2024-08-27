#' Connect to the database
#'
#' @description
#' Connect to the database.
#' To disconnect, call `disconnect_database()`.
#'
#' @param user The user name (default: "admin").
#' @param password The password (default: "admin").
#' @param host The host name (default: "localhost").
#' @param port The port number (default: 5432).
#' @param dbname The database name (default: "roadnetdb").
#' @return A connection to the database.
#' @export
#' @examples
#' \dontrun{
#' connect_database()
#' }
connect_database <- function(user = "admin", password = "p@ssw0rd",
                             host = "localhost", port = 5432,
                             dbname = "roadnetdb") {
  con <- try(dbConnect(Postgres(),
                       user = user,
                       pass = password,
                       host = host,
                       port = port,
                       dbname = dbname), silent = TRUE)
  if (inherits(con, "try-error")) {
    cli_alert_danger(c("Failed to connect to the database. ",
                       "Is the database up? ",
                       "database: {dbname}, user: {user}, ",
                       "host: {host}, port: {port}"),
                     wrap = TRUE)
    return(invisible(NULL))
  }
  cli_alert_success("Connected to the database: {dbname}")
  return(invisible(con))
}
