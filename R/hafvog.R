# check https://github.com/jennybc/manipulate-xml-with-purrr-dplyr-tidyr
# https://ryouready.wordpress.com/2009/01/23/r-combining-vectors-or-data-frames-of-unequal-length-into-one-data-frame/

xml_detangler <- function(x) {

  lh2 <- function(x) {
    cn <- x %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
    x <- x %>% xml2::xml_children() %>% xml2::xml_text()
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


#' Title
#'
#' @param xmlfile filename
#'
#' @return list
#' @export
#'
read_hafvog <- function(xmlfile) {

  res <- list()

  # ----------------------------------------------------------------------------
  # stack the russian doll

  res <- list()

  for (i in 1:length(xmlfile)) {

    #print(paste(i, xmlfile[i]))

    st <- read_hafvog_station(xmlfile[i])
    le <- read_hafvog_lengths(xmlfile[i])
    if(nrow(le) > 0) le$stod_nr <- st$stod_nr
    ot <- read_hafvog_individuals(xmlfile[i])
    if(nrow(ot) > 0) ot$stod_nr <- st$stod_nr
    ss <- read_hafvog_counts(xmlfile[i])
    if(!is.null(ss)) ss$stod_nr <- st$stod_nr

    if(i == 1) {
      res$st <- st
      res$le <- le
      res$ot <- ot
      res$ss <- ss
    } else {
      res$st                  <- dplyr::bind_rows(res$st, st)
      if(nrow(le) > 0) res$le <- dplyr::bind_rows(res$le, le)
      if(nrow(ot) > 0) res$ot <- dplyr::bind_rows(res$ot, ot)
      if(!is.null(ss)) res$ss <- dplyr::bind_rows(res$ss, ss)
    }
  }

  # ----------------------------------------------------------------------------
  # compile the length table (from length and otoliths)
  #le <- d$le %>%
  #  select(stod_nr, species, length, n) %>%
  #  bind_rows(d$ot %>% select(stod_nr, species, length))

  tmp <- tempfile()
  write.csv(res$ot, tmp, row.names = FALSE)
  res$ot <- read.csv(tmp, stringsAsFactors = FALSE,dec = ",")



  return(res)
}

#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
#' @export
#'
read_hafvog_station <- function(xmlfile) {
  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all("STOD") %>%
    xml2::xml_children()

  x2 <- x %>% xml2::xml_text()
  names(x2) <- x %>% xml2::xml_name() %>% tolower()
  d <- as.data.frame(t(x2), stringsAsFactors = FALSE)
  tmp <- tempfile()
  write.csv2(d, tmp, row.names = FALSE)
  d <-
    read.csv2(tmp, stringsAsFactors = FALSE) %>%
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


#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
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
  d <- read.csv2(tmp, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df()

  return(d)
}

#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
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
  d <- read.csv2(tmp, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df()
  return(d)
}

#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
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
  d <- read.csv2(tmp, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df()


  return(d)
}
