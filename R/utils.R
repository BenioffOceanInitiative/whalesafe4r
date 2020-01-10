
#' Determines if an object is a url
#'
#' \code{is.url_only}
#'
#' @param x The object to test
#'
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


#' @export
is.txt_file <- function(x){
  grepl("\\.txt$", x, perl = TRUE, ignore.case = TRUE)
}

#' Returns the url path
#'
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
