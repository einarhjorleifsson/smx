#' Read from table fiskar.stodvar or fjolst::stodvar
#'
#' @param year Years to include
#' @param month Months to include
#' @param cruise.id Cruises to include
#' @param gear.id Gear to include
#' @param rect Statistical rectangle to include
#' @param id Station id (synis.id) to include
#' @param col.names Column names to return
#' @param oracle Boolean, if TRUE (default) obtains data from database,
#' otherwise from the binary data in fjolst.
#'
#' @return A dataframe
#' @export
#'
read_stations <- function(year = NULL,
                          month = NULL,
                          cruise.id = NULL,
                          gear.id = NULL,
                          rect = NULL,
                          id = NULL,
                          col.names,
                          oracle = TRUE) {

  d <- fjolst::lesa.stodvar(ar = year, man = month, leidangur = cruise.id,
                           veidarfaeri = gear.id,
                           reitur = rect,
                           synis.id = id,
                           col.names = col.names,
                           oracle = oracle) %>%
    dplyr::tbl_df() %>%
    dplyr::rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)]))

  return(d)
}


#' Title
#'
#' @param id station id
#' @param species species id
#' @param col.names column names to return
#' @param sex include sex
#' @param mat include maturity
#' @param length specify length
#' @param corrected xx
#' @param oracle xx
#'
#' @return A dataframe
#' @export
#'
read_lengths <- function(id, species, col.names = c("id","length","n"),
                         sex = NULL, mat = NULL, length = NULL,
                         corrected = F, oracle = TRUE) {

  d <- fjolst::lesa.lengdir(id, teg = species,
                            kyn = sex,
                            kynthroski = mat,
                            lengd = length,
                            leidrett = corrected,
                            col.names = c("synis.id", "tegund", "lengd", "fjoldi"),
                            oracle = oracle) %>%
    dplyr::tbl_df() %>%
    dplyr::rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)]))

  return(d)
}


#' Title
#'
#' @param id xxx
#' @param species xxx
#' @param weight xxx
#' @param corrected xxx
#' @param oracle xxx
#'
#' @return A dataframe
#' @export
#'
read_subsampling <- function(id, species, weight = FALSE,
                             corrected = FALSE,
                             oracle = TRUE) {
 d <- fjolst::lesa.numer(id, teg = species,
                         vigt = weight,
                         col.names =c("synis.id", "tegund", "fj.maelt", "fj.talid"),
                         leidrett = corrected,
                         oracle = oracle) %>%
   dplyr::tbl_df() %>%
   dplyr::rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)])) %>%
   dplyr::mutate(r = 1 + n.counted/n.measured) %>%
   dplyr::select(-n.counted, -n.measured)

 return(d)

}

