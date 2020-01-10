
#' Recursively dive into directories, find urls, fix them, and return the child-most
#' path

.get_links <- function(u){
  if(is.txt_file(u)){
    return(u)
  }else if(is.url_only(u)){
    u <- xml2::read_html(u)
  }
  links <- xml2::xml_find_all(u, ".//a[not(contains(@href,'../'))]")
  links <- xml2::xml_attr(links, "href")
  links <- xml2::url_absolute(links, xml2::xml_url(u))
  return(links)
}

#' Recursively call .get_links
whale.dive <- function(u){
  links <- .get_links(u)
  if(!all(is.txt_file(links))){
    unlist(lapply(links, whale.dive))
  }else {
    links <- unlist(links, recursive = TRUE, use.names = FALSE)
    return(links)
  }
}

#' Build an xpath selector pragmatically from a provided URL
#'
#'
#' @examples
#' > .next_xpath("https://ais.sbarc.org/logs_delimited/2018/181212/")
#'
#' [1] ".//a[contains(.,'181212') or contains(@href, '181212')]//following-sibling::a[@href]"
.next_xpath <- function(pat = NULL, ...){
  if(is.url_only(pat)){
    pat <- basename(pat)
  }else if(is.html_doc(pat)){
    pat <- basename(xml2::xml_url(pat))
  }
  sprintf(".//a[contains(.,'%s') or contains(@href, '%s')]//following-sibling::a[@href]", pat, pat)
}

#' Find siblings, and build urls from a url
#'
#'
#' @param start_url The URL the crawler begins it's call from, ie, our last URL from the logfile
#'
#' .find_link_sibs("https://ais.sbarc.org/logs_delimited/2018/181212/")
#'
#' > .find_link_sibs(("https://ais.sbarc.org/logs_delimited/2018/181231/")) # Returns NULL
#'
#'
.find_link_sibs <- function(start_url = NULL){

    base_pat <- basename(start_url)

    find_xpath <- .next_xpath(base_pat)

    call_url <- url.drop_basename(start_url)

    raw_doc <- xml2::read_html(call_url)

    next_nodes <- xml2::xml_find_all(raw_doc, find_xpath)

    get_hrefs <- xml2::xml_attr(next_nodes, "href")

    base_url_path <-  xml2::xml_url(raw_doc)

    new_urls <- sprintf("%s/%s", gsub("\\/$", "", base_url_path, perl = TRUE), get_hrefs)

    if(!length(new_urls)){
      return(NULL)
    }else {
      return(new_urls)
    }

}



#' Get latest AIS URLs since last given URL to start
#'
#' This will take our start url, find sibs, then walk recursively UP the tree
#' and find the next directories if any avail...
#'
#' basically it works like this:
#'
#' It will forcefully find all siblings then recurse back down the tree until it
#' finds all the txt files
#'
#' @param start_url The path to a TXT url. See notes and examples
#'
#' @note THIS REQUIRES THE START URL TO BE A TXT FILE PATH
#'
#' @examples
#'
#' get_ais_urls("https://ais.sbarc.org/logs_delimited/2018/180408/AIS_SBARC_180408-12.txt")
#'
#'

get_ais_urls <- function(start_url){

  if(is.txt_file(start_url)){

    l <- .find_link_sibs(start_url = start_url)

    start_url <- url.drop_basename(start_url)

    l <- append(l, .find_link_sibs(start_url = start_url))

    start_url <- url.drop_basename(start_url)

    l <- append(l, .find_link_sibs(start_url = start_url))

    # these are directories
    dive_into <- l[which(!is.txt_file(l))]

    l <- l[which(is.txt_file(l))]

    append(l, unlist(lapply(dive_into, .get_links)))
  }

}



#
#
# whale.dive_ll <- function(u, n_cores = parallel::detectCores()){
#   links <- .get_links(u)
#   if(!all(is.txt_file(links))){
#     unlist(parallel::mclapply(links, whale.dive, mc.cores = n_cores))
#   }else {
#     links <- unlist(links, recursive = TRUE, use.names = FALSE)
#     return(links)
#   }
# }
#
#
#

#

#
# .has_next_sib <- function(x, xpath = NULL, ...){
#   if(!is.html_doc(x) && is.url_only(x)){
#     x <- xml2::read_html(x)
#   }
#
# }
#
# .make_xpath <- function(which_type = c('top', 'next_sibling'), pat = NULL){
#
#   bases <- list(
#     top = "not(contains(@href,'../'))",
#     next_sibling = "contains(.,'%s')]//following-sibling::a"
#   )
#
#   sprint_this <- match.arg(which_type)
#
#   sprintf(".//a[%s]", bases[sprint_this], pat)
#
# }
#
