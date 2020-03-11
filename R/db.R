
#' Connect to database
#'
#' @param yml yaml metadata for connecting. should contain parameters for: host, database, user, port, password
#'
#' @return con database connection object
#' @importFrom here here
#' @importFrom yaml yaml.load_file
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#' 
#' @export
db_connect <- function(db_yml = here::here("s4w_amazon_rds.yml")){

  # connect ----
  #db_yml <- file.path(dir_gdata, "amazon_rds.yml")
  db <- yaml::yaml.load_file(db_yml)

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = db$database,
    host     = db$host,
    port     = db$port,
    user     = db$user,
    password = db$password,
    sslmode  = 'require')

  return(con)
}
