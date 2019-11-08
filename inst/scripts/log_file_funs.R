#' Log file creation
#'
#' \code{logfile.set_path}
#'
#' Important as this will store the logfile path as a global param, rather than
#' require us to pass to each function. If the file doesn't exist, or isn't found,
#' it will prompt you for next steps. auto_create will simply force a new file
#' to be created and then set.
#'
#' @param file_location The path to the logfile on your local system or a shared repo etc
#'
#' @param auto_create Logical indicating if the logfile should be created if not found
#'
#' @param global_name Environment label, which will be used to also find later...
#'
#' @return
#' Sets the global environment variable to the file path... which we then simply
#' call as \code{Sys.getenv("whale_log")}. NOTE that you can change this variable name to whatever...
#' just update other functions that call it
#'
#'
#'
#' @author Carl Boneri, Sean Goral
#'
#'
#' @examples
#' If creating a new logfile, you can call without providing any arguments
#' logfile.set_path()
#'
#'
#' For testing purposess...
#' 1 call Sys.getenv(whale_log) and see if output is empty
#' 2 then run logfile.set_path() and follow prompts if needed
#' 3 call Sys.getenv(whale_log)
#'
#' @export

logfile.set_path <- function(file_location = NULL, auto_create = FALSE, global_name = "whale_log", ...){

  if(is.null(file_location) || !file.exists(file_location)){

    if(auto_create){

      o <- file.create(file_location,showWarnings = FALSE)

    }else {

      what_todo <- select.list(
        choices = c(TRUE, FALSE),
        title = "Didn't find that file, do you want to create a new file?"
      )

      if(!what_todo){
        return("Try another file path")
      }else {
        if(is.null(file_location)){
          file_location <- readline("give me a file name please...")
        }
        file.create(file_location, showWarnings = FALSE)
      }

    }

  }
  # setting globally... change whale_log to whatever, but know that we are going
  # to use Sys.getenv(whatever_name) to retrieve the path later...
  e_val <- setNames(list(base::normalizePath(file_location)), global_name)

  do.call(Sys.setenv, e_val)

}



#' Finding the logfile
#'
#' \code{logfile.get_path}
#'
#' If \code{logfile.set_path} has been called, this will find the variable globally,
#' if not it returns NULL, because we don't want it breaking our parser
#'
#' @examples
#' logfile.get_path()
#'
logfile.get_path <- function(global_name = "whale_log"){
  val <- Sys.getenv(global_name)
  if(!nchar(val)){
    NULL
  }else {
    val
  }
}


#' Function to update the logs
#'
#'
#' The logfile is what will track and update which links have been visited and
#' already read-in/parsed to our tables.
#'
#' The advantage of this approach is that we:
#' \enumerate{
#'   \item remove the potential of duplicating data in the master table
#'   \item reduce the number of requests we make to the server
#'   \item apply our parsing function to only data not already present, so much less time/computational requirement
#' }
#'
#'


logfile.update <- function(log_file_path = NULL, url_path = NULL, has_been_read = FALSE, date_checked = Sys.time(),...){

  # Need the file path to write to, can set with logfile.set_path or provide any filepath
  if(is.null(log_file_path)){
    log_file_path <- logfile.get_path()
  }

  if(!is.null(date_checked) && !inherits(date_checked, "POSIXct")){
    date_checked <- as.POSIXct(date_checked, "%Y-%m-%d %H:%M:%S")
  }

  df <- data.frame(url_path = url_path, has_been_read = has_been_read, date_checked = date_checked)

}



logfile.last_checked_url <- function(log_data = NULL){
  log_data[nrow(log_data), 'url_path']
}


logfile.next_url <- function(...){

}
