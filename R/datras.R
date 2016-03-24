#' @title Tidy the datras station (HH) data
#'
#' @description Creates a station id
#'
#' @param hh datras station table
#' @param to.lower A boolean, if TRUE (default) then column names are all turned
#' to lower.
#'
#' @return A dataframe
#' @export
#'
tidy_station <- function(hh, to.lower = TRUE) {

  old.names <- names(hh)
  hh <- setNames(hh, tolower(names(hh)))

  # unique station id
  hh <-
    hh %>%
    dplyr::tbl_df() %>%
    #dplyr::mutate(id = paste(year, quarter, country, ship, gear, stno, haulno,sep="-"))
    #  this seems to be sufficient
    dplyr::mutate(id = paste(year, quarter, ship, gear, haulno, sep = "-"))

  # proper date/time format
  hh <-
    hh %>%
    dplyr::mutate(timeshot = ifelse(nchar(timeshot) < 4,
                                    paste0(stringr::str_sub("000",1,4-nchar(timeshot)),timeshot),
                                    as.character(timeshot)),
                  timeshot = paste0(substr(timeshot,1,2),":",substr(timeshot,3,4)),
                  date = lubridate::ymd_hm(paste(year,month,day,timeshot)),
                  datehaul = date + 60 * hauldur) %>%
    dplyr::select(-month, -day, -timeshot)

  # needs work:
  # if(!to.lower) hh <- setNames(hh, c(old.names,"id","date","datehaul"))

  return(hh)

}

#' Title
#'
#' @param hl datras length frequency table
#' @param hh datras station table (cleaned)
#' @param correct_raising A boolean, if TRUE reconstruct original counts
#'
#' @return A dataframe
#' @export
#'
tidy_length <- function(hl, hh, correct_raising = FALSE) {

  old.names <- names(hl)
  hl <- setNames(hl, tolower(names(hl)))

  hl <-
    hl %>%
    #dplyr::mutate(id = paste(year, quarter, country, ship, gear, stno, haulno,sep="-"),
    # this seems to be sufficient
    dplyr::mutate(id = paste(year, quarter, ship, gear, haulno, sep = "-"),
                  valid_aphia = as.character(valid_aphia)) %>%
    dplyr::select(-(recordtype:year))

  # standardize/recode length
  hl <-
    hl %>%
    dplyr::mutate(lngtclass = as.numeric(lngtclass),
                  lngtclass = ifelse(lngtcode %in% c(".","0"),
                              0.1 * lngtclass,
                              lngtclass)) %>%
    dplyr::select(-lngtcode)

  # reconstruct the original count-variable in the length table
  #  has been standaridized to 1 hour if datatype field in
  #  the station table is labelled C (R is no standardization
  #  has been done)

  # and then do the raising
  if(correct_raising) {
    hl <-
      hl %>%
      dplyr::left_join(hh %>% dplyr::select(id, datatype, hauldur), by = "id") %>%
      # Where datatype is C the hlnoatlngt has been standardized to 60 minutes tow time
      # The number of fish in the hl table have also been raised by the raising factor
      # Here the original measurements are "recovered"
      dplyr::mutate(hauldur = ifelse(is.na(hauldur),30, hauldur),
             n = ifelse(datatype == "C", hlnoatlngt * hauldur/60, hlnoatlngt),
             r = ifelse(is.na(subfactor), 1, subfactor),
             n = n)
  }

  return(hl)

}
