#' Connetion to hafriti
#'
#' @param password Your password
#'
#' @return A SQL connection
#' @export
#'
hr_connection <- function(password) {
  RPostgres::dbConnect(drv = RPostgres::Postgres(),
                 user = 'hafriti_user',
                 password = password,
                 host = 'hfs-lipgsql01.hafogvatn.is',
                 dbname = 'skip')
}

