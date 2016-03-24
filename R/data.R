#' @name dictionary
#' @title A dictionary of column names
#' @description Icelandic (fjolst and oracle) and english variable names
#' @docType data
#' @format dataframe
NULL

#' @name LENGDIR
#' @title The length classes by species
#' @description A list whose names corresponds to species.id and whose element
#' are the length classes used to compute survey indices.
#' @docType data
#' @format list
#' @source
#' \code{
#' attach('~/stasi/Splus5/SMB/.RData')
#' use_data(LENGDIR)
#' detach('file:~/stasi/Splus5/SMB/.RData')
#' }
#' @author Hoskuldur Bjornsson
NULL

#' @name lwc
#' @title Length weigth coefficients
#' @description A \emph{dataframe} that contains length-weight coefficients
#' (a and b) for a number of species. Same data as \link{LWCOEFF}, the latter
#' being a list.
#' @docType data
#' @format dataframe
#' @source The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' names(LWCOEFF) <- iconv(names(LWCOEFF), "ISO8859-1", "UTF-8")
#' attributes(LWCOEFF$"12") <- NULL
#' lwc <- plyr::ldply(LWCOEFF)
#' names(lwc) <- c("species.id", "a","b")
#' lwc <-
#'   lwc %>%
#'   tbl_df() %>%
#'   mutate(species.id = as.integer(species.id)) %>%
#'   filter(!is.na(species.id))
#' use_data(lwc, overwrite = TRUE)
#' }
#' @author Hoskuldur Bjornsson
NULL

#' @name LWCOEFF
#' @title Length weigth coefficients
#' @description A \emph{list} that contains length-weight coefficients
#' (a and b) for a number of species. Same data as \link{lwc}, the latter
#' being a dataframe.
#' @docType data
#' @format dataframe
#' @source The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/.RData")
#' names(LWCOEFF) <- iconv(names(LWCOEFF), "ISO8859-1", "UTF-8")
#' attributes(LWCOEFF$"12") <- NULL
#' use_data(LWCOEFF)
#' }
#' @author Hoskuldur Bjornsson
NULL

#' @name orri_veidarfaeri
#' @title Check if really needed
#' @description XXX
#' @docType data
#' @format dataframe
#' @source Oracle database
#' @author Einar Hjorleifsson
NULL

#' @name Std.aggregation
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' names(Std.aggregation) <- iconv(names(Std.aggregation),'ISO8859-1','UTF-8')
#' use(Std.aggregation)
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL

#' @name stratas_new
#' @title Area of new strata
#' @description Name, area size and central coordinate position of new strata
#' of the Icelandic groundfish survey.
#' @docType data
#' @format dataframe
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/export/u2/reikn/Splus5/HAUSTRALLNewStrata/Stratifiering/.RData". It
#' was copied into package via:
#' \code{
#' attach("/net/hafkaldi/export/u2/reikn/Splus5/HAUSTRALLNewStrata/Stratifiering/.RData")
#' x <- attributes(STRATAS)
#' stratas_new <-
#'   dplyr::data_frame(newstrata = 1:45,
#'                     area = as.numeric(x$area),
#'                     rall.area = x$rall.area,
#'                     lon = x$pos$lon,
#'                     lat = x$pos$lat,
#'                     .Names = x$names)
#' devtools::use_data(stratas_new)
#' }
#' @author Hoskuldur Bjornsson (hoski@hafro.is)
NULL

#' @name stratas_old
#' @title Area of old strata
#' @description Name, area size and central coordinate position of old strata
#' of the Icelandic groundfish survey.
#' @docType data
#' @format dataframe
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' was copied into package via:
#' \code{
#' attach("~/stasi/Splus5/SMB/GEOMETRY.NEW/.RData")
#' x <- attributes(STRATAS)
#' stratas_old <- data_frame(oldstrata = as.integer(x$names),
#'                                  area = as.numeric(x$area),
#'                                  rall.area = x$rall.area,
#'                                  lon = x$pos$lon,
#'                                  lat = x$pos$lat,
#'                                  .Names = as.integer(x$.Names))
#' devtools::use_data(stratas_old)
#' }
#' @author Hoskuldur Bjornsson (hoski@hafro.is)
NULL

