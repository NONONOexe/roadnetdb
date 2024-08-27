#' Shutdown the database
#'
#' @description
#' Shutdown the database using docker.
#' This function requires docker to be installed beforehand.
#'
#' @return The return code of this process.
#' @export
#' @examples
#' \dontrun{
#' shutdown_database()
#' }
shutdown_database <- function() {
  compose_file <- system.file("docker/compose.yaml", package="roadnetdb")
  command <- paste("docker compose -p roadnetdb -f", compose_file, "down")
  cli_alert_info("Execute the command: {command}")
  code <- shell(command)
  if (code != 0) {
    cli_alert_danger("Failed to launch the database (return code: {code}).")
  } else {
    database <- Sys.getenv("POSTGRES_DB")
    cli_alert_success("The database is shutdowned: {database}")
  }
  return(invisible(code))
}
