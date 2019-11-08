library(RCurl)

curl.whales <- function(urls = NULL, db_name = NULL, port = NULL, user = NULL, password = NULL){

  results <- list()

  success <- function(x){
    results <<- append(results, list(x))
  }

  failure <- function(str){
    cat(paste("Failed request:", str), file = stderr())
  }

  o <- lapply(urls, function(i){
    multi_add(new_handle(url = i), done = success, fail = failure)
  })

  multi_run()

  results <- unlist(lapply(results, function(j){
    d <- rawToChar(j[['content']])
    d <- stringi::stri_split_regex(d, "\r\n") %>% unlist
    need_fixes <- grep("\\;\\;", d, perl = T, value = T)
    d[need_fixes] <- stringi::stri_replace_all_regex(d[need_fixes], "(?<=\\;)(?=\\;)", "NA", vectorize_all = F)
    d
  }))
  #return(results)
  #
  # ALL SQL stuffs go below here
  # 1) setup connection
  # 2) write to connection
  # 3) close connection
  # whale.write_sql(results, ...)
}



whale.split <- function(x){
  x <- stringi::stri_split_regex(x, "\r?\n", omit_empty = TRUE) %>% unlist
  x
}

whale.read <- function(fpath = NULL){

}
