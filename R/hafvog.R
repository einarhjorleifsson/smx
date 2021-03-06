# check https://github.com/jennybc/manipulate-xml-with-purrr-dplyr-tidyr
# https://ryouready.wordpress.com/2009/01/23/r-combining-vectors-or-data-frames-of-unequal-length-into-one-data-frame/

xml_detangler <- function(x) {

  lh2 <- function(x) {
    cn <- x %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
    x <- x %>% xml2::xml_children() %>% xml2::xml_text()
    x <- x %>% stringr::str_replace_all(",", ".") %>% as.numeric()
    names(x) <- cn
    return(x)
  }

  x <- x %>% lapply(lh2)
  d <- do.call(rbind, lapply(lapply(x, unlist), "[",
                             unique(unlist(c(sapply(x,names))))))
  d <- as.data.frame(d, stringsAsFactors = FALSE) %>% dplyr::tbl_df()
  names(d) <- unique(unlist(c(sapply(x,names))))
  return(d)
}


#' @title Reads data from Hafvog
#'
#' @description Reads in xml-files generated by Hafvog, an electronic measurement
#' system used at Marine Research Institute, Reykavík.
#'
#' @param xmlfile Character vector specifying the name(s), including path
#' (location) of the xml-files.
#'
#' @return A list with the following dataframes:
#' \itemize{
#' \item st: Station table
#' \item le: Length frequency table
#' \item ot: Individual measurement table
#' \item ss: Subsampling (numer) table
#' }
#' @export
#'
read_hafvog <- function(xmlfile) {

  st <- le <- ss <- ot <- vector("list", length(xmlfile))

  # ----------------------------------------------------------------------------
  # stack the russian doll


  for (i in seq_along(xmlfile)) {


    #print(paste(i, xmlfile[i]))

    x <- read_hafvog_station(xmlfile[i])
    if(!is.null(x)) st[[i]] <- x

    x <- read_hafvog_lengths(xmlfile[i])
    if(!is.null(x)) {
      x$stod_nr <- st[[i]]$stod_nr
      le[[i]] <- x
    }

    x <- read_hafvog_individuals(xmlfile[i])
    if(!is.null(x)) {
      x$stod_nr <- st[[i]]$stod_nr
      ot[[i]] <- x
    }

    x <- read_hafvog_counts(xmlfile[i])
    if(!is.null(x)) {
      x$stod_nr <- st[[i]]$stod_nr
      ss[[i]] <- x
    }
  }

  x <- vector("list", 4)
  names(x) <- c("st","le","ot","ss")
  #x$st <- plyr::ldply(st) %>% tbl_df()
  x$st <- st %>% purrr::reduce(dplyr::as_data_frame)
  x$le <- le %>% purrr::reduce(dplyr::as_data_frame)
  x$ot <- ot %>% purrr::reduce(dplyr::as_data_frame)
  x$ss <- ss %>% purrr::reduce(dplyr::as_data_frame)

  return(x)
}

#' @title Reads station data from Hafvog
#'
#' @description Reads in station information from xml-files generated by
#' Hafvog, an electronic measurement system used at Marine Research Institute,
#' Reykavík.
#'
#' @param xmlfile Character vector specifying the name, including path
#' (location) of the xml-files.
#'
#' @return dataframe
#'
#' @export
#'
read_hafvog_station <- function(xmlfile) {
  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all("STOD") %>%
    xml2::xml_children()

  x2 <-
    x %>%
    xml2::xml_text()
  names(x2) <- x %>% xml2::xml_name() %>% tolower()

  d <- as.data.frame(t(x2), stringsAsFactors = FALSE)
  tmp <- tempfile()
  write.csv(d, tmp, row.names = FALSE)
  d <- read.csv(tmp, stringsAsFactors = FALSE, dec = ",") %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(dags      = lubridate::ymd(dags),
                  togbyrjun = lubridate::ymd_hm(togbyrjun),
                  togendir  = lubridate::ymd_hm(togendir),
                  lon1 = -gisland::geo_convert(kastad_v_lengd),
                  lat1 =  gisland::geo_convert(kastad_n_breidd),
                  lon2 = -gisland::geo_convert(hift_v_lengd),
                  lat2 =  gisland::geo_convert(hift_n_breidd),
                  lon = (lon1 + lon2)/2,
                  lat = (lat1 + lat2)/2) %>%
    dplyr::tbl_df()

  return(d)
}


#' @title Reads length data from Hafvog
#'
#' @description Reads in length frequencies from xml-files generated by
#' Hafvog, an electronic measurement system used at Marine Research Institute,
#' Reykavík.
#'
#' @param xmlfile Character vector specifying the name, including path
#' (location) of the xml-files.
#'
#' @return dataframe
#'
#' @export
#'
read_hafvog_lengths <- function(xmlfile) {

  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all('SKRANING') %>%
    xml2::xml_find_all('MAELIADGERD')

  d <-
    x[[1]] %>%
    xml2::xml_children()  %>%
    xml2::as_list() %>%
    xml_detangler()

  # kyngreind lengdarmaeling
  d2 <-
    x[[2]] %>%
    xml2::xml_children()  %>%
    xml2::as_list() %>%
    xml_detangler

  d <- dplyr::bind_rows(d, d2)

  if(nrow(d) == 0) return(NULL)

  tmp <- tempfile()
  write.csv2(d, tmp, row.names = FALSE)
  d <- read.csv2(tmp, stringsAsFactors = FALSE, dec = ",") %>%
    dplyr::tbl_df()

  return(d)
}

#' @title Reads biological data from Hafvog
#'
#' @description Reads in individual measurements from xml-files generated by
#' Hafvog, an electronic measurement system used at Marine Research Institute,
#' Reykavík.
#'
#' @param xmlfile Character vector specifying the name, including path
#' (location) of the xml-files.
#'
#' @return dataframe
#'
#' @export
#'
read_hafvog_individuals <- function(xmlfile) {

  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all('SKRANING') %>%
    xml2::xml_find_all('MAELIADGERD')

  d <-
    x[[3]] %>%
    xml2::xml_children()  %>%
    xml2::as_list() %>%
    xml_detangler

  if(nrow(d) == 0) return(NULL)

  tmp <- tempfile()
  write.csv2(d, tmp, row.names = FALSE)
  d <- read.csv2(tmp, stringsAsFactors = FALSE, dec = ",") %>%
    dplyr::tbl_df()
  return(d)
}

#' @title Reads subsampling data from Hafvog
#'
#' @description Reads in subsamplin data from xml-files generated by
#' Hafvog, an electronic measurement system used at Marine Research Institute,
#' Reykavík.
#'
#' @param xmlfile Character vector specifying the name, including path
#' (location) of the xml-files.
#'
#' @return dataframe
#'
#' @export
#'
read_hafvog_counts <- function(xmlfile) {

  #print(xmlfile)

  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all('SKRANING') %>%
    xml2::xml_find_all('MAELIADGERD')

  d <- x[[6]] %>%
    xml2::xml_children()  %>%
    xml2::as_list() %>%
    xml_detangler()

  if(nrow(d) == 0) return(NULL)

  tmp <- tempfile()
  write.csv2(d, tmp, row.names = FALSE)
  d <- read.csv2(tmp, stringsAsFactors = FALSE, dec = ",") %>%
    dplyr::tbl_df()


  return(d)
}
