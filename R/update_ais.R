
#' Update AIS data and Spatial Features data in postgres database.  TODO: HANDLE ERRORS, WARNINGS, EMPTY TXT FILE URLs
#'
#' @return
#' @export
#'
#' @examples
#' cred_path = "/Users/seangoral/github/s4wr/s4w_amazon_rds.yml"
#'
update_ais_data <- function(){
  # initiate connection
  con =db_connect()
  # Create "log" dataframe in R from log_df table in database
  log = dbGetQuery(con, "SELECT * FROM log_df;")
  # Subtract 'row.names' column
  log = log %>% select(-row.names)
  # Get last read ais.txt file
  last_read = logfile.last_url(log)
  # Get list of new links by giving get_ais_urls() funcction last_read
  new_links = get_ais_urls(last_read)
  # Create "log_df" in R by binding new_links (unread) with "log" dataframe
  log_df <- rbind(log, data.frame(url = new_links, is_read = F, timestamp = as.numeric(Sys.time())))

  # Loop through "new_links" and update "log_df"
  new_data <- parallel::mclapply(tst, function(url){
    df <-  tryCatch(whale.reader(path = url, log_df = log_df, assign_back = TRUE),
                    # ... but if an error occurs, tell me what happened:
                    error=function(error_message) {
                      message(" - Empty or corrupted ais.txt files.")
                      message("And below is the error message from R:")
                      message(error_message)
                      return(NA)
                    })
          assign(url, df)
  }, mc.cores = 8)
  # row bind "new_data"
  DF = do.call(rbind,new_data)

  # Loop through "new_links" and create segments spatial data
  new_sf_data <-  parallel::mclapply(tst, function(url){
    df1 <-  tryCatch(shippy_lines(path = url), error=function(e) NULL)
    assign(url, df1)
  }, mc.cores = 8)
  # Row bind "new_sf_data"
  SF_DF = do.call(rbind,new_sf_data)


  #write ais_data to 'ais_data' table in database
  dbWriteTable(con, name = 'ais_data_testy', value = DF, append=T)
  #write ais_SF_data to 'ais_segments' table in database
  dbWriteTable(con, name = 'ais_segments', value = SF_DF, append=T)
  #overwrite log_df in database with updated log_df from R environment
  dbWriteTable(con, name = 'log_df_test', value = log_df, overwrite=TRUE)
  #close database connection
  dbDisconnect(con)

  return(DF)

  }


system.time({
test=update_ais_data()
})

# 24 links with purrr:map
# user    system  elapsed
# 19.377   1.103  55.691

# 24 links with parallel::mclapply
# user    system  elapsed
# 29.057   4.235  28.198

## 65 links
# user    system    elapsed
# 46.221   2.608    127.761
