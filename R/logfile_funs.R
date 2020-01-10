

 log_df_test <- data.frame(
   url = character(),
   is_read = logical(),
   timestamp = as.POSIXct(character()),
   stringsAsFactors = T
 )

 #all_links = whale.dive(u = "https://ais.sbarc.org/logs_delimited/")

# o <- whale.dive(u = "https://ais.sbarc.org/logs_delimited/2019/")
# o2 <- whale.dive(u = "https://ais.sbarc.org/logs_delimited/2018/")
 
# log_df_test <- rbind(log_df_test, data.frame(url = all_links, is_read = F, timestamp = as.numeric(Sys.time())))
#
#saveRDS(list(whale_links_2018 = o2, whale_links_2019 = o, test_df = log_df_test), file = "inst/data/sandbox_data.rds")
#
# > readRDS("inst/data/sandbox_data.rds") %>% names()
# [1] "whale_links_2018" "whale_links_2019" "test_df"
#
# testing json stream
#
#write_json(log_df_test, auto_unbox = TRUE, path = "inst/data/logfile.json")



#' Log data manipulation
#'
#' \code{logfile.update}
#'
#' This function handles the processing of our log data. The log data will tell
#' our other functions what needs to be read, what has been read and where to
#' start/stop. Additionally the time-stamps will provide a little insight into
#' debugging if data is missing etc in the future.
#'
#' You can import the log data from a database and pass to the \code{log_df}
#' parameter, which if you then set \code{assign_back} to TRUE, will reassign the updated
#' dataframe back into the work-space, and can be written back to the database after. See examples
#' for a rough walk-through.
#'
#' Additionally you can provide a path to a json file for processing. This will avoid
#' the need to add any tables to the working environment, and will assign, in place
#' the dataframe, and write to the same connection upon closing.
#'
#' If a url already exists, but we are changing the status of the data being read,
#' the data is updated in place, and the cell for \code{is_read} simply changes.
#'
#'
#'
#' @param log_df The table/data.frame containing the log data
#' @param url the url to either be updated or appended
#' @param is_read logical indicating whether or not the url has already been read in
#' @param assign_back logical indicating whether the data.frame should be reassigned back to the
#'   working environment
#' @param logfile_path If provided and the log_df param is NULL, this file path will be
#'   processed into the working environment/function as the log_df. Upon exit, the new data
#'   will be written to the file location. At time of build/testing there is a logfile
#'   located at \strong{\code{inst/data/logfile.json}}
#' @param ... additional arguments to be processed
#'
#' @return
#'
#'
#' @examples
#'
#' FOR DB connection
#'
#' Create the object in R as the_log 
#' the_log <- dbConnect(your_tables)
#'
#' this will reassign the new data to the_log, note that if assign_back = FALSE,
#' the entire data.frame will be returned, so catch with the_log <- logfile.update(log_df = the_log)
#' logfile.update(log_df = the_log, assign_back = TRUE)
#'
#' write back to db
#' writeDB(the_log, ...)
#'
#' Example if the data is in the environment as log_df_test with only 4 rows
#' nrow(log_df_test)
#' [1] 4
#'
#' > tail(log_df_test,1)
#'                                                                        url is_read  timestamp
#' 4 https://ais.sbarc.org/logs_delimited/2018/180101/AIS_SBARC_180101-03.txt   FALSE 1574145802
#'
#' > logfile.update(log_df = log_df_test, url = "testurl.com", is_read = TRUE, assign_back = TRUE)
#' > tail(log_df_test,2)
#'                                                                        url is_read  timestamp
#' 4 https://ais.sbarc.org/logs_delimited/2018/180101/AIS_SBARC_180101-03.txt   FALSE 1574145802
#' 5                                                              testurl.com    TRUE 1574148016
#'
#' > nrow(log_df_test)
#' [1] 5
#'
#' @export
#' 
logfile.update <- function(log_df = NULL, url = NULL, is_read = FALSE, assign_back = FALSE,
                           logfile_path = NULL, ...){

  # getting the name of whatever data.frame object is used in case reassigning globally
  # with the assign_back param...
  df_name <- deparse(substitute(log_df))


  if(is.null(log_df) || !is.data.frame(log_df)){
    if(!is.null(logfile_path) && file.exists(logfile_path)){
      log_df <- fromJSON(logfile_path)
      df_name <- "log_df"
    }else {
      return("need data.frame for log_df")
    }
  }

  # first detect if the url we are checking for is in the data.frame
  # already
  # set to 2 to ignore case
  hits <- which(stringi::stri_cmp_equiv(url, log_df$url, strength = 2))

  the_time <- as.numeric(Sys.time())

  if(any(hits)){
    log_df[hits, c('is_read', 'timestamp')] <- list(is_read, the_time)
  }else {
    log_df <- rbind(log_df, data.frame(url = url, is_read = is_read, timestamp = the_time))
  }

  if(!is.null(logfile_path)){
    write_json(log_df, auto_unbox = TRUE, path = logfile_path)
  }

  if(assign_back){
    assign(df_name, log_df, envir = .GlobalEnv)
  }else {
    return(log_df)
  }

}


#Gets the next unread url from the log_df, which is where crawler will begin
logfile.next_url <- function(log_df){
  log_df %>% filter(!is_read) %>% .$url
}

#new_links <- logfile.next_url(log_df = log_last_unread)
logfile.last_url <- function(log_df){
  tail(log_df$url,n=1)
}
#test = logfile.last_url(log_df)
