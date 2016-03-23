#' @name smbSTODVAR
#' @title Synis.id and strata of the spring groundfish survey (SMB)
#' @description Contains only the strata for each tow, both the new and the
#' old stratas. The old strata is obtained from the column newstrata in
#' STODVAR located at
#' /net/hafkaldi/export/u2/reikn/Splus5/SMB/.RData. The newstrata is from
#' column newstrata in STODVAR located at /net/hafkaldi/export/u2/reikn/Splus5/SMBNewstrata/.RData
#' @docType data
#' @format dataframe
NULL

#' @name dictionary
#' @title Some title
#' @description Icelandic and english variable names
#' @docType data
#' @format dataframe
NULL

#' @name LENGDIR
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source
#' \code{
#' attach('/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')
#' save('LENGDIR',file='data/LENGDIR.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL

#' @name LWCOEFF
#' @title Length weigth coefficients
#' @description A \emph{list} that contains length-weight coefficients for number of species.
#' @docType data
#' @format Each element of the list contains parameter a and b for different
#' species. The name of the element of the list corresponds to numerical species
#' code.
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach('/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')
#' save('LWCOEFF',file='data/LWCOEFF.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Hoskuldur Bjornsson
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
#' save(Std.aggregation,file='data/Std.aggregation.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL

#' @name stratas_old_attributes
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' x <- attributes(STRATAS)
#' stratas_attributes <- data_frame(oldstrata = as.integer(x$names),
#'                                  area = as.numeric(x$area),
#'                                  rall.area = x$rall.area,
#'                                  lon = x$pos$lon,
#'                                  lat = x$pos$lat,
#'                                  .Names = as.integer(x$.Names))
#' devtools::use_data(stratas_attributes)
#' }
#' @author Hoskuldur Bjornsson (hoski@hafro.is)
NULL

#' @name stratas_new_attributes
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/export/u2/reikn/Splus5/HAUSTRALLNewStrata/Stratifiering/.RData")
#' x <- attributes(STRATAS)
#' stratas_new_attributes <-
#'   dplyr::data_frame(newstrata = 1:45,
#'                     area = as.numeric(x$area),
#'                     rall.area = x$rall.area,
#'                     lon = x$pos$lon,
#'                     lat = x$pos$lat,
#'                     .Names = x$names)
#' devtools::use_data(stratas_new_attributes)
#' }
#' @author Hoskuldur Bjornsson (hoski@hafro.is)
NULL

#' @name STRATAS
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' for (i in 1:length(STRATAS)) {
#'   attributes(STRATAS[[i]])$name <- iconv(attributes(STRATAS[[i]])$name,
#'                                          'ISO8859-1','UTF-8')}
#' save(STRATAS,file='data/STRATAS.rda')
#' detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")}
#' @author Some author
NULL

#' @name STRATAS
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' for (i in 1:length(STRATAS)) {
#'   attributes(STRATAS[[i]])$name <- iconv(attributes(STRATAS[[i]])$name,
#'                                          'ISO8859-1','UTF-8')}
#' save(STRATAS,file='data/STRATAS.rda')
#' detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")}
#' @author Some author
NULL


#' @name orri_veidarfaeri
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @format Some format
#' @source
#' \code{
#' attach('/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')
#' save('LENGDIR',file='data/LENGDIR.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL
