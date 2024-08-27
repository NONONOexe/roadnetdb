#' Launch the database
#'
#' @description
#' Launch the database using docker.
#' This function requires docker to be installed beforehand.
#'
#' @param user The user name (default: "admin").
#' @param password The password (default: "p@ssword").
#' @param port The port number (default: 5432).
#' @param dbname The database name (default: "roadnetdb").
#' @return The return code of this process.
#' @export
#' @examples
#' \dontrun{
#' launch_database()
#' }
launch_database <- function(user = "admin", password = "p@ssw0rd",
                            dbname = "roadnetdb", port = 5432) {
  # Set environment variables
  set_environment_variables(user, password, dbname, port)

  # Create the data volume
  cli_alert_info("Creating the docker volume for the database")
  code <- create_data_volume(dbname)
  if (code != 0L) {
    return(invisible(code))
  }

  # Launch the container
  cli_alert_info("Launching the docker container of the database")
  code <- launch_container()
  if (code != 0) {
    return(invisible(code))
  }
  cli_alert_success("The database is launched: {dbname}")

  return(invisible(code))
}

set_environment_variables <- function(user, password, dbname, port) {
  Sys.setenv("POSTGRES_USER" = user)
  Sys.setenv("POSTGRES_PASSWORD" = password)
  Sys.setenv("POSTGRES_PORT" = port)
  Sys.setenv("POSTGRES_DB" = dbname)
}

create_data_volume <- function(dbname) {
  command <- paste0("docker volume create ", dbname, "_data")
  cli_alert_info("Executing the command: {command}")
  code <- suppressWarnings(shell(command))
  if (code == 1L) {
    cli_alert_danger("Failed to execute command")
  }
  return(code)
}

launch_container <- function() {
  compose_file <- system.file("docker/compose.yaml", package = "roadnetdb")
  command <- paste("docker compose -p roadnetdb -f", compose_file, "up -d")
  cli_alert_info("Executing the command: {command}")
  code <- suppressWarnings(shell(command))
  if (code == 1L) {
    cli_alert_danger("Failed to execute command")
  }
  return(code)
}
