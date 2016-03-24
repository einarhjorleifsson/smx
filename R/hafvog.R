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

    #print(i)

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

  x2 <-
    x[[1]] %>%
    xml2::xml_children()  %>%
    xml2::as_list()

  for (k in 1:length(x2)) {
    cn <- x2[[k]] %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
    x2[[k]] <- x2[[k]] %>% xml2::xml_children() %>%  xml2::xml_text()
    names(x2[[k]]) <- cn
  }

  d <- do.call(rbind, lapply(lapply(x2, unlist), "[",
                             unique(unlist(c(sapply(x2,names))))))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- unique(unlist(c(sapply(x2,names))))

  # kyngreind lengdarmaeling
  x2 <-
    x[[2]] %>%
    xml2::xml_children()  %>%
    xml2::as_list()

  if(length(x2) > 0) {
    for (k in 1:length(x2)) {
      cn <- x2[[k]] %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
      x2[[k]] <- x2[[k]] %>% xml2::xml_children() %>%  xml2::xml_text()
      names(x2[[k]]) <- cn
    }

    d2 <- do.call(rbind, lapply(lapply(x2, unlist), "[",
                               unique(unlist(c(sapply(x2,names))))))
    d2 <- as.data.frame(d2, stringsAsFactors = FALSE)
    names(d2) <- unique(unlist(c(sapply(x2,names))))

    d <- dplyr::bind_rows(d, d2)
  }



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

  x <- x[[3]]

  # Trial -------------------------------------------------------------------
  # https://ryouready.wordpress.com/2009/01/23/r-combining-vectors-or-data-frames-of-unequal-length-into-one-data-frame/
  x1 <-
    x %>%
    xml2::xml_children()  %>%
    xml2::xml_children()  %>%
    xml2::xml_text()
  names(x1) <-
    tolower(x  %>%
              xml2::xml_children()  %>%
              xml2::xml_children()  %>%
              xml2::xml_name())
  cn <- unique(names(x1))

  x2 <-
    x %>%
    xml2::xml_children()  %>%
    xml2::as_list()

  for (k in 1:length(x2)) {
    cn <- x2[[k]] %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
    x2[[k]] <- x2[[k]] %>% xml2::xml_children() %>%  xml2::xml_text()
    names(x2[[k]]) <- cn
    #cn <- paste(names(x2[[k]]), collapse = ":")
    #x2[[k]] <- paste(x2[[k]], collapse = ":")
    #names(x2[[k]]) <- cn
  }

  d <- do.call(rbind, lapply(lapply(x2, unlist), "[",
                              unique(unlist(c(sapply(x2,names))))))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- unique(unlist(c(sapply(x2,names))))

  #tmp <- tempfile()
  #write.csv(d, file=tmp, row.names = F)
  #d <- read.csv(tmp, stringsAsFactors = FALSE)

  #raw <- matrix(NA,
  #              nrow = xml2::xml_length(x),
  #              ncol = length(cn),
  #              dimnames=list(1:xml2::xml_length(x),cn))
  #for(j in 1:xml2::xml_length(x)) {
  #}


  #d <- dplyr::data_frame(rnr = x %>% xml2::xml_find_all(xpath = ".//RADNR") %>% xml2::xml_text() %>% as.integer(),
  #                        species = x %>% xml2::xml_children() %>% xml2::xml_find_all(xpath = ".//TEGUND") %>% xml2::xml_text() %>% as.integer(),
  #                        nr = x %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
  #                        length = x %>% xml2::xml_find_all(xpath = ".//LENGD") %>% xml2::xml_text() %>% as.integer()) %>%
  #  dplyr::arrange(species, nr, rnr) %>%
  #  dplyr::mutate(n = 1)

  #tmp <- tempfile()
  #write.csv2(d, tmp, row.names = FALSE)
  #d <- read.csv2(tmp, stringsAsFactors = FALSE) %>%
  #  dplyr::tbl_df()
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

  x <- x[[6]]
  x1 <-
    x %>%
    xml2::xml_children()  %>%
    xml2::xml_children()  %>%
    xml2::xml_text()
  names(x1) <-
    tolower(x  %>%
              xml2::xml_children()  %>%
              xml2::xml_children()  %>%
              xml2::xml_name())
  cn <- unique(names(x1))

  x2 <-
    x %>%
    xml2::xml_children()  %>%
    xml2::as_list()

  if(length(x2) > 0) {

  for (k in 1:length(x2)) {
    cn <- x2[[k]] %>% xml2::xml_children() %>% xml2::xml_name() %>% tolower()
    x2[[k]] <- x2[[k]] %>% xml2::xml_children() %>%  xml2::xml_text()
    names(x2[[k]]) <- cn
    #cn <- paste(names(x2[[k]]), collapse = ":")
    #x2[[k]] <- paste(x2[[k]], collapse = ":")
    #names(x2[[k]]) <- cn
  }

  d <- do.call(rbind, lapply(lapply(x2, unlist), "[",
                             unique(unlist(c(sapply(x2,names))))))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- unique(unlist(c(sapply(x2,names))))


  #d <- dplyr::data_frame(rnr = x %>% xml2::xml_find_all(xpath = ".//RADNR") %>% xml2::xml_text() %>% as.integer(),
  #                        species = x %>% xml2::xml_children() %>% xml2::xml_find_all(xpath = ".//TEGUND") %>% xml2::xml_text() %>% as.integer(),
  #                        nr = x %>% xml2::xml_find_all(xpath = ".//NR") %>% xml2::xml_text() %>% as.integer(),
  #                        n = x %>% xml2::xml_find_all(xpath = ".//FJOLDI") %>% xml2::xml_text() %>% as.integer())
  tmp <- tempfile()
  write.csv2(d, tmp, row.names = FALSE)
  d <- read.csv2(tmp, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df()
  return(d)
  }
  return(NULL)
}
