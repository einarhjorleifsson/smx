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
