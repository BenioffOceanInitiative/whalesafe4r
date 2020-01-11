
#' Update AIS data in postgres database
#'
#' @return
#' @export
#'
#' @examples
#' cred_path = "/Users/seangoral/github/s4wr/s4w_amazon_rds.yml"
#'
update_ais_data <- function(){
  #create connection
  con =db_connect()

  log = dbGetQuery(con, "SELECT * FROM log_df;")
  log = log %>% select(-row.names)

  last_read = logfile.last_url(log)

  new_links = get_ais_urls(last_read)

  log_df <- rbind(log, data.frame(url = new_links, is_read = F, timestamp = as.numeric(Sys.time())))

  #loop through new_links and update log_df
  new_data <- purrr::map(new_links, function(url){
    df <-  tryCatch(whale.reader(path = url, log_df = log_df, assign_back = TRUE), error=function(e) NULL)
    assign(url, df)
  })

  DF = do.call(rbind,new_data)

  new_sf_data <- purrr::map(tst, function(url){
    df <-  tryCatch(shippy_lines(path = url), error=function(e) NULL)
    assign(url, df)
  })

  SF_DF = do.call(rbind,new_sf_data)

  dbWriteTable(con, name = 'ais_data_testy', value = DF, append=T)

  dbWriteTable(con, name = 'log_df_test', value = log_df, overwite=T)

  dbDisconnect(con)

  return(DF)

  }


system.time({
test=update_ais_data()
})

