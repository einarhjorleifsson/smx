#' @title XXX
#'
#' @description XXX
#'
#' @export
#'
#' @param st A dataframe containing columns id, year, towlength and strata
#' @param le A dataframe containing columns id, length and n where the latter are the "raised" numbers
#' @param lwcoeff A vector containing the length weight coefficients a and b.
#' @param stratas, A dataframe containing columns strata and area.
#' @param std.towlength Tow length in nautical miles
#' @param std.cv Default is 1.
#' @param std.area Standardized area swept in nautical square miles
#'
calc_indices <- function(st,
                         le,
                         lwcoeff = c(0.01, 3),
                         stratas,
                         std.towlength = 4,
                         std.cv = 1,
                         std.area = 4 * 17/1852) {

  # dummy, for passing test without lot of notes
  id <- n <- towlength <- b <- year <- strata <- N <- n_m <- cn <-
    cn_m <- b_m <- cb <- cb_m <- area <- n_d <- b_d <- cn_d <- cb_d <- NULL

  # Because we are calculating the abundance less than and biomass greater than
  #  we first generate a data.frame based on all combination of synis.id and length
  #  classes
  base <-
    dplyr::as_data_frame(expand.grid(length = c(5:140), id = st$id)) %>%
    dplyr::left_join(st, by = "id") %>%
    dplyr::left_join(le, by=c("id","length")) %>%
    dplyr::arrange(id, length) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n  = ifelse(is.na(n),0,n)  * std.towlength / towlength, # standardized to per 4 miles
           cn = cumsum(n),
           b  = n * lwcoeff[1] * length^lwcoeff[2]/1e3,
           cb = sum(b) - cumsum(b) + b) %>%
    dplyr::group_by(year, strata, length) %>%
    dplyr::summarise(N  = n(),
              n_m  = mean(n),
              n_d  = ifelse(N == 1, n_m  * std.cv, sd(n)),
              cn_m = mean(cn),
              cn_d = ifelse(N == 1, cn_m * std.cv, sd(cn)),
              b_m  = mean(b),
              b_d  = ifelse(N == 1, b_m  * std.cv, sd(b)),
              cb_m = mean(cb),
              cb_d = ifelse(N == 1, cb_m * std.cv, sd(cb))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(stratas %>% dplyr::select(strata, area = area), by = "strata") %>%
    dplyr::mutate(area  = area/1.852^2 / std.area,
           n     = n_m  * area,
           cn    = cn_m * area,
           b     = b_m  * area,
           cb    = cb_m * area)

  aggr <-
    base %>%
    dplyr::group_by(year, length) %>%
    # A la Höski:
    dplyr::summarise(n = sum(n),
              n.cv = calc_cv(n_m,n_d,area,N),
              b = sum(b),
              b.cv = calc_cv(b_m,b_d,area,N),
              cn = sum(cn),
              cn.cv = calc_cv(cn_m, cn_d, area, N),
              cb = sum(cb),
              cb.cv = calc_cv(cb_m, cb_d, area, N))

  return(list(base = base, aggr = aggr))

}

calc_cv <- function(x, xd, area, N) {
  Mean = sum(x * area)/sum(area)
  Sum = sum(x * area)
  tmpsum = sum(x[!is.na(xd)] * area[!is.na(xd)])
  Calc.sdev = sqrt(sum(xd[!is.na(xd)]^2 * area[!is.na(xd)]^2/  N[!is.na(xd)])   / sum(area[!is.na(xd)])^2)
  Sdev = Calc.sdev * Sum/tmpsum
  cv = Sdev/Mean

  return(cv)
}

#' @title Trim extreme towlengths
#'
#' @description Function that "trims" towlength to a certain minimum and/or maximum length
#'
#' @export
#'
#' @param x A vector representing towlength
#' @param std.towlength Standard towlength (default is 4)
#' @param min.towlength Minimum towlength. If missing (default) the value is set to
#' half of the std.towlength
#' @param max.towlength Maximum towlength. If missing (default) the value is set to
#' double the std.towlength
trim_towlength <- function(x, std.towlength = 4, min.towlength, max.towlength) {

  if(missing(min.towlength)) min.towlength <- std.towlength / 2
  if(missing(max.towlength)) max.towlength <- std.towlength * 2

  x <- ifelse(is.na(x),std.towlength, x)
  x <- ifelse(x > max.towlength, max.towlength, x)
  x <- ifelse(x < min.towlength, min.towlength, x)

  return(x)
}
