#' Title
#'
#' @param year
#' @param month
#' @param cruise.id
#' @param gear.id
#' @param rect
#' @param id
#' @param oracle
#'
#' @return
#' @export
#'
#' @examples
read_stations <- function(year = NULL,
                          month = NULL,
                          cruise.id = NULL,
                          gear.id = NULL,
                          rect = NULL,
                          id = NULL,
                          #col.names = stodvar.col,
                          oracle = TRUE) {

  d <- fjolst::lesa.stodvar(ar = year, man = month, leidangur = cruise.id,
                           veidarfaeri = gear.id,
                           reitur = rect,
                           synis.id = id,
                           oracle = oracle) %>%
    tbl_df() %>%
    rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)]))

  return(d)
}


#' Title
#'
#' @param id
#' @param species
#' @param col.names
#' @param sex
#' @param mat
#' @param length
#' @param corrected
#' @param oracle
#'
#' @return
#' @export
#'
#' @examples
read_lengths <- function(id, species, col.names = c("id","length","n"),
                         sex = NULL, mat = NULL, length = NULL,
                         corrected = F, oracle = TRUE) {

  d <- fjolst::lesa.lengdir(id, teg = species, #note col.names just default
                            kyn = sex,
                            kynthroski = mat,
                            lengd = length,
                            leidrett = corrected,
                            oracle = oracle) %>%
    tbl_df() %>%
    rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)]))

  return(d)
}


#' Title
#'
#' @param id
#' @param species
#' @param weight
#' @param corrected
#' @param oracle
#'
#' @return
#' @export
#'
#' @examples
read_subsampling <- function(id, species, weight = FALSE,
                             corrected = FALSE,
                             oracle = TRUE) {
 d <- fjolst::lesa.numer(id, teg = species, vigt = weight,
                         leidrett = corrected,
                         oracle = oracle) %>%
   tbl_df() %>%
   rename_(.dots=setNames(names(.),dictionary$eng[match(names(.),dictionary$ice)])) %>%
   mutate(r = 1 + n.counted/n.measured) %>%
   select(-n.counted, -n.measured)

 return(d)

}

