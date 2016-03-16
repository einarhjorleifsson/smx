#' Title
#'
#' @param xmlfile filename
#'
#' @return list
#' @export
#'
read_hafvog <- function(xmlfile) {

  st <- read_hafvog_station(xmlfile)
  names(st) <- tolower(names(st))
  le <- read_hafvog_lengths(xmlfile)
  le$stod_nr <- st$stod_nr
  ot <- read_hafvog_individuals(xmlfile)
  if(nrow(ot) > 0) ot$stod_nr <- st$stod_nr
  ss <- read_hafvog_counts(xmlfile)
  if(nrow(ss) > 0) ss$stod_nr <- st$stod_nr

  x <- list(st = st, le = le, ot = ot, ss = ss)
  return(x)

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
  names(x2) <- x %>% xml2::xml_name()
  df <- as.data.frame(t(x2), stringsAsFactors = FALSE)

  return(df)
}


#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
#' @export
#'
#' @examples
read_hafvog_lengths <- function(xmlfile) {

  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all('SKRANING') %>%
    xml2::xml_find_all('MAELIADGERD')

  le <- x[[1]]
  df <- dplyr::data_frame(rnr = le %>% xml2::xml_find_all(xpath = ".//RADNR") %>% xml2::xml_text() %>% as.integer(),
                          species = le %>% xml2::xml_children() %>% xml2::xml_find_all(xpath = ".//TEGUND") %>% xml2::xml_text() %>% as.integer(),
                          nr = le %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
                          length = le %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
                          n = le %>% xml2::xml_find_all(xpath = ".//FJOLDI") %>% xml2::xml_text() %>% as.integer()) %>%
    dplyr::arrange(species, nr, rnr)
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

  x <- x[[3]]
  df <- dplyr::data_frame(rnr = x %>% xml2::xml_find_all(xpath = ".//RADNR") %>% xml2::xml_text() %>% as.integer(),
                          species = x %>% xml2::xml_children() %>% xml2::xml_find_all(xpath = ".//TEGUND") %>% xml2::xml_text() %>% as.integer(),
                          nr = x %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
                          length = x %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer()) %>%
    dplyr::arrange(species, nr, rnr)
}

#' Title
#'
#' @param xmlfile filename
#'
#' @return dataframe
#' @export
#'
read_hafvog_counts <- function(xmlfile) {

  x <-
    xml2::read_xml(xmlfile) %>%
    xml2::xml_find_all("SYNI") %>%
    xml2::xml_find_all('SKRANING') %>%
    xml2::xml_find_all('MAELIADGERD')

  x <- x[[6]]
  df <- dplyr::data_frame(rnr = x %>% xml2::xml_find_all(xpath = ".//RADNR") %>% xml2::xml_text() %>% as.integer(),
                          species = x %>% xml2::xml_children() %>% xml2::xml_find_all(xpath = ".//TEGUND") %>% xml2::xml_text() %>% as.integer(),
                          nr = x %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
                          n = x %>% xml2::xml_find_all(xpath = ".//FJOLDI") %>% xml2::xml_text() %>% as.integer())
  #if(nrow(df) == 0) return(NULL)
  #%>%
    #dplyr::arrange(species, nr, rnr)
  #return(df)
  return(df)
}
