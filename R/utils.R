
#' Determines if an object is a url
#'
#' \code{is.url_only}
#'
#' @param x The object to test
#'
#' @examples
#' u <- 'https://ais.sbarc.org/logs_delimited/2019/190202/AIS_SBARC_190202-04.txt'
#' > is.url_only(x = u)
#' [1] TRUE
#'
#' raw <- xml2::read_html(u)
#' > is.url_only(x = raw)
#' [1] FALSE
#'
#' > is.url_only(xml_find_all(raw,".//a[1]"))
#' [1] FALSE
#'
#' @export
is.url_only <- function(x){
  if(is.character(x) && !is.html_doc(x = x) && !is.html_nodeset(x = x)){
    TRUE
  }else {
    FALSE
  }
}


#' Is an object an xml_document
#'
#' \code{is.html_doc}
#'
#' @param x The object to test
#'
#' @examples
#' u <- 'https://ais.sbarc.org/logs_delimited/2019/190202/AIS_SBARC_190202-04.txt'
#' > is.html_doc(u)
#' [1] FALSE
#' > is.html_doc(raw)
#' [1] TRUE
#' > is.html_doc(xml_find_all(raw,".//a[1]"))
#' [1] FALSE
#'
#' @export
is.html_doc <- function(x){
  inherits(x, 'xml_document')
}


#' Determine if an html_nodeset
#'
#' @param x The object to test
#'
#'
#' @examples
#' > is.html_nodeset(xml_find_all(raw,".//a[1]"))
#' [1] TRUE
#' > is.html_nodeset(raw)
#' [1] FALSE
#' > is.html_nodeset(u)
#' [1] FALSE
#'
#' @export
is.html_nodeset <- function(x){
  inherits(x, "xml_nodeset")
}


#' Extracts the url forcefully
#'
#' Need this to ensure a few steps later on
#'
#' @export
url.get <- function(x){
  if(is.url_only(x)){
    return(x)
  }else if(is.html_doc(x)){
    x <- xml2::xml_url(x)
    return(x)
  }else {
    return(NA)
  }
}

#' check if url is text file
#' 
#' @export
is.txt_file <- function(x){
  grepl("\\.txt$", x, perl = TRUE, ignore.case = TRUE)
}

#' Returns the url path
#
#' @export
url.path <- function(url = NULL, ...){
  url <- url.get(x = url)
  if(!is.na(url)){
    gsub("https?\\:\\/\\/(.*?)(?=/)", "", url, perl = TRUE)
  }else {
    return(NA)
  }
}


#' Returns the hostname/protocol portion
#'
#' @examples
#' u <- "https://ais.sbarc.org/logs_delimited/2019/190202/AIS_SBARC_190202-04.txt"
#' > url.hostname(u)
#' [1] "https://ais.sbarc.org/"
#'
#' @export
url.hostname <- function(url = NULL, ...){

  url <- url.get(url)

  if(!is.na(url)){
    unlist(stringi::stri_extract_all_regex(url, "https?\\:\\/\\/(.*?)\\/"))
  }else {
    return(NA)
  }

}

#' Drops the basename
#'
#' Needed for walking back up the tree
#'
#' @examples
#'  u <- "https://ais.sbarc.org/logs_delimited/2019/190202/AIS_SBARC_190202-04.txt"
#'
#' @export
url.drop_basename <- function(url = NULL, ...){

  url <- url.get(url)

  url <- gsub(sprintf("/%s", base::basename(url)), "", url, perl = TRUE)

  url
}

# Check file size from url function ----
#' @param path (url path)
#' @return size of the file 
#' @examples
#' tst1 = check_url_file_size("https://ais.sbarc.org/logs_delimited/2019/191109/AIS_SBARC_191109-18.txt")
#' 74251
#' tst2 = check_url_file_size("https://ais.sbarc.org/logs_delimited/2019/191130/AIS_SBARC_191130-00.txt")
#' 0
#' @export
check_url_file_size <- function(path){
  response = httr::HEAD(path)
  file_size=as.numeric(httr::headers(response)[["Content-Length"]])
  return(file_size)
}

# Date from url handlers ----

#' Parse and build the Ymd from the url/filename
#' @export
date.from_filename <- function(fname){
  str <- stringi::stri_extract_all_regex(basename(fname), "[0-9]+(?=\\-)") %>% unlist
  strptime(as.numeric(sprintf("20%s", str)), "%Y%m%d") %>% as.character()
}

#' Reformat the timestamp for fractionals ----
#' @param t 
#'
#' @return timestamp
#' @export
date.as_frac <- function(t){
  if(!grepl("\\.", t, perl = TRUE)){
    stringi::stri_replace_last_regex(t, "\\:", ".")
  }else {
    return(t)
  }
}

#' Build the datestring object ----
#' @param ymd year,month,day
#' @param ts timestamp
#'
#' @export
date.build <- function(ymd = NULL, ts = NULL){
  options(digits.secs = 12)
  if(!grepl("\\.", ts, perl = TRUE)){
    ts <- date.as_frac(ts)
  }
  pat <- sprintf("%s %s", ymd, ts)
  as.POSIXct(pat, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

# Spatial functions ----

#' Get kilometer Length from segment
#' @param segment (AIS Segment)
#' @export
get_length_km <- function(segment){
  # seg <- p$segment[2]
  if (is.na(segment)) return(NA)
  
  st_length(segment) %>%
    set_units("km") %>%
    drop_units()
}

#' Build segment from AIS Messages
#'
#' @param p1 point 1
#' @param p2 point 2
#' @param crs (4326)
#'
#' @return segment
#' @export
get_segment <- function(p1, p2, crs=4326){
  
  if (any(is.na(p1), is.na(p2))) return(NA)
  
  st_combine(c(p1, p2)) %>%
    st_cast("LINESTRING") %>%
    st_set_crs(crs)
}