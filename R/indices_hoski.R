#' @title Calculate length based indices
#'
#' @description Calculates abundance and biomass survey indices based on length
#' classes for a particular species in a given year.
#'
#' @export
#' @param lengths \emph{data.frame} containing length measurement for a given
#' species in a given survey year.
#' @param stations \emph{data.frame} containg station survey station informations.
#' @param sex \emph{boolean} A flag indicating if indices should also be
#' compiled by sex
#' @param lenClass \emph{numerical vector} containing length classes to
#' compile the indices for
#' @param yr \emph{numerical} value for year
#' @param lwcoeff \emph{numerical vector} of length 2, containing parameter
#' a and b of the length weight relationship.
#' @param ... Other parameters to pass to Calc.index
bioIndex <- function(lengths,stations,sex,lenClass,yr,lwcoeff, ...) {
  for( i in 1:length(lenClass)) {
    L <- lengths

    # A.1 total number of fish of a length class by station
    le <- L[L$lengd==lenClass[i],]
    if(nrow(le) > 0) {
      x <- apply.shrink(le$fj.alls,le$synis.id,sum)
      names(x) <- c("synis.id","fj")
      st <- join(stations[,c("newstrata","toglengd","synis.id")],x,"synis.id",set=0)
    } else {
      st <- stations[,c("newstrata","toglengd","synis.id")]
      st$fj <- 0
    }
    # A.2 total number of fish of a length class by station and sex
    if(sex) {
      le <- L[L$lengd==lenClass[i] & L$kyn==1 & !is.na(L$kyn),]
      if(nrow(le) > 0) {
        x <- apply.shrink(le$fj.alls,le$synis.id,sum)
        names(x) <- c("synis.id","fjhaenga")
        st <- join(st,x,"synis.id",set=0)
      } else {
        st$fjhaenga <- 0
      }
      le <- L[L$lengd==lenClass[i] & L$kyn==2 & !is.na(L$kyn),]
      if(nrow(le) > 0) {
        x <- apply.shrink(le$fj.alls,le$synis.id,sum)
        names(x) <- c("synis.id","fjhrygna")
        st <- join(st,x,"synis.id",set=0)
      } else {
        st$fjhrygna <- 0
      }
    }

    # B. Biomass GREATER than a certain length class
    le <- L[L$lengd >= lenClass[i],]
    if(nrow(le) > 0) {
      x <- apply.shrink(le$bio,le$synis.id,sum)
      names(x) <- c("synis.id","bioge")
      st <- join(st,x,"synis.id",set=0)
    } else {
      st$bioge <- 0
    }

    # C. Abundance LESS than a certain length class
    le <- L[L$lengd <= lenClass[i],]
    if(nrow(le) > 0) {
      x <- apply.shrink(le$fj.alls,le$synis.id,sum)
      names(x) <- c("synis.id","fjle")
      st <- join(st,x,"synis.id",set=0)
    } else {
      st$fjle <- 0
    }

    # D. Calculate the indices
    visit <- Calc.index(st,"fj")
    B0 <- Calc.index(st,"bioge")
    N0 <- Calc.index(st,"fjle")
    if(sex) {
      N0sex1 <- Calc.index(st,"fjhaenga")
      N0sex2 <- Calc.index(st,"fjhrygna")
    }

    base0 <- visit$result[,c("strata","total","cv")]
    names(base0)[2:3] <- c("fj","cv.fj")
    base0$bio.staerri <- B0$result[,"total"]
    base0$cv.bio.staerri <- B0$result[,"cv"]
    base0$fj.minni <- N0$result[,"total"]
    base0$cv.fj.minni <- N0$result[,"cv"]

    if(sex) {
      base0$fj.haenga <- N0sex1$result[,"total"]
      base0$cv.fj.haenga <- N0sex1$result[,"cv"]
      base0$fj.hrygna <- N0sex2$result[,"total"]
      base0$cv.fj.hrygna <- N0sex2$result[,"cv"]
    }

    aggr0 <- visit$aggr.output[,c("total","cv")]
    names(aggr0) <- c("fj","cv.fj")
    aggr0$bio.staerri <- B0$aggr.output[,"total"]
    aggr0$cv.bio.staerri <- B0$aggr.output[,"cv"]
    aggr0$fj.minni <- N0$aggr.output[,"total"]
    aggr0$cv.fj.minni <- N0$aggr.output[,"cv"]

    if(sex) {
      aggr0$fj.haenga <- N0sex1$aggr.output[,"total"]
      aggr0$cv.fj.haenga <- N0sex1$aggr.output[,"cv"]
      aggr0$fj.hrygna <- N0sex2$aggr.output[,"total"]
      aggr0$cv.fj.hrygna <- N0sex2$aggr.output[,"cv"]
    }

    aggr0$svaedi <- dimnames(aggr0)[[1]]
    aggr0$svaedisnr <- 1:nrow(aggr0)
    dimnames(aggr0)[[1]] <- 1:nrow(aggr0)
    base0$lengd <- lenClass[i]
    aggr0$lengd <- lenClass[i]
    base0$ar <- yr
    aggr0$ar <- yr

    if(i == 1 ) {
      base <- base0
      aggr <- aggr0
    } else {
      base <- rbind(base,base0)
      aggr <- rbind(aggr,aggr0)
    }
  } # end of lenClass loop

  aggr$bio <- aggr$fj*lwcoeff[1]*aggr$lengd^lwcoeff[2]/1e3
  base$bio <- base$fj*lwcoeff[1]*base$lengd^lwcoeff[2]/1e3

  outdata <- list(base=base,aggr=aggr)
  return(outdata)
}

#' Calculate survey indices
#'
#' Original function from Hoski. This function is used in \code{\link{bioIndex}}.
#' The function does in principle three things:
#' \itemize{
#' \item Standardizes value (e.g. number of fish) by tow length..
#' \item Calculates stratified indices.
#' \item Aggregates the stratified indices in various ways.
#' }
#'
#' @export
#' @param sfile \emph{data.frame} Station table that contains columns
#' \code{synis.id} and \code{newstrata} for the \code{z}-file. Note that
#' the dataframe does not have to include abundance ....
#' @param colname Column to be used as the basis for the calculation. These
#' and be \code{fj}, ....
#' @param strata.list \emph{list} that contains the stratas to be used as the
#' basis for the calculation. Norally use \code{strata.list=ralllist}.
#' @param std.toglengd \emph{numeric} The length of the standard tow, default
#' is 4 (miles).
#' @param trollbreidd The swept width, default is 17. The area swept of each tow
#' is then calculated as \code{std.area <- (std.toglengd * trollbreidd)/1852}
#' resulting in standardized area swept of the tow in square miles.
#' @param combine.output \emph{list} that contains list of stratas upon which
#' the calculation should be aggregated. Norally (default) use Std.aggregation.
#' @param use.rallarea \emph{boolean} If TRUE (default) then strata areas
#' specified in STRATAS$rall.area is used. If FALSE
#' strata areas specified in STRATAS$area is used.
#' @param STRATAS Standard stuff
#' @param leidretta.fyrir.toglengd \emph{Boolean} Default TRUE
#' \itemize{
#' \item if \code{toglengd} in \code{sfile} \emph{NA} then set \code{toglengd}
#' to \code{std.toglengd}
#' \item if \code{toglengd} in \code{sfile} is less than \code{mintoglengd}
#' then set \code{toglengd} to \code{mintoglengd}
#' \item if \code{toglengd} in \code{sfile} is greater than \code{maxtoglengd}
#' then set \code{toglengd} to \code{maxtoglengd}
#' }
#' @param z DO NOT UNDERSTAND THIS
#' @param std.cv \emph{numerical} The default coefficient of variation. This
#' value will be used in cases when cv can not be calculated.
#' @param mintoglengd \emph{numerical} Value repaces \code{toglengd} in \code{sfile}
#' if the latter is smaller than stated (and if \code{leidretta.fyrir.toglengd=TRUE}.
#' @param maxtoglengd Value repaces \code{toglengd} in \code{sfile}
#' if the latter is greater than stated (and if \code{leidretta.fyrir.toglengd=TRUE}.
#' @return Returns a \emph{list} with the following \emph{data.frame}s:
#' \itemize{
#' \item \code{$result} with the following columns:
#' \itemize{
#' \item \code{strata}: Names/number of the strata
#' \item \code{mean}: Mean (number or biomass) within the strata
#' \item \code{stdev}: Standard deviation within the strata
#' \item \code{count}: Number of tows within the strata
#' \item \code{area}: The area (sqmiles) of the strata
#' \item \code{se}: Standard error
#' \item \code{cv}: Coefficient of variation
#' \item \code{total}: Total (number or biomass) in the strata.
#' }
#' \item \code{$Res.names} Contains information about the strata names
#' \item \code{$Aggr.output} with the following columns:
#' \itemize{
#' \item \code{mean}: Mean (number or biomass) within an area
#' \item \code{se}: Standard error within an area
#' \item \code{count}: Number of stations within an area ??????
#' \item \code{area}: The area (sqmiles) of the area
#' \item \code{total}: Total (number or biomass) in the area.
#' }
#' }
#'
Calc.index <- function (sfile,
                        colname,
                        strata.list,
                        std.toglengd = 4,
                        trollbreidd = 17,
                        combine.output = Std.aggregation,
                        use.rallarea = T,
                        STRATAS = STRATAS,
                        leidretta.fyrir.toglengd = T,
                        z,
                        std.cv = 1,
                        mintoglengd,
                        maxtoglengd) {


  sfile$strata <- sfile$newstrata

  if (!missing(z)) {
    sfile$outcome <- z
    colname <- "outcome"
  }

  # 1) Standardize values by tow length (see index.standardize)
  if (missing(mintoglengd)) mintoglengd <- std.toglengd/2
  if (missing(maxtoglengd)) maxtoglengd <- std.toglengd * 2

  std.area <- std.toglengd * trollbreidd/1852

  if (!is.na(match("toglengd", names(sfile))) && leidretta.fyrir.toglengd)
  {
    i <- is.na(sfile$toglengd)
    if (any(i)) sfile$toglengd[i] <- std.toglengd
    i <- sfile$toglengd < mintoglengd
    if (any(i)) sfile$toglengd[i] <- mintoglengd
    i <- sfile$toglengd > maxtoglengd
    if (any(i)) sfile$toglengd[i] <- maxtoglengd
    sfile[, colname] <- sfile[, colname] * std.toglengd/sfile$toglengd
  }


  if (use.rallarea)
  {
    areas <- attributes(STRATAS)$rall.area
  } else {
    areas <- attributes(STRATAS)$area
  }

  Names <- attributes(STRATAS)$name

  if (!missing(strata.list))
  {
    for (i in 1:length(strata.list))
    {
      areas[strata.list[[i[1]]]] <- sum(areas[strata.list[[i]]])
      j <- !is.na(match(sfile$strata, strata.list[[i]]))
      if (any(j))
        sfile$strata[j] <- strata.list[[i]][1]
    }
  }

  tmp6 <- apply.shrink(sfile[, colname], sfile$strata, mean)
  tmp7 <- apply.shrink(sfile[, colname], sfile$strata, sd)
  tmp8 <- apply.shrink(rep(1, nrow(sfile)), sfile$strata, sum)

  result <- data.frame(strata = tmp8[, 1], mean = tmp6[, 2],
                       sdev = tmp7[, 2], count = tmp8[, 2])

  names(result) <- c("strata", "mean", "sdev", "count")

  i <- result$count == 1
  if (any(i)) result$sdev[i] <- result$mean[i] * std.cv
  # Note: A correction is made here
  # was: result$area <- areas[result$strata]/1.854^2
  result$area <- areas[result$strata]/1.852^2
  result$se <- result$sdev/sqrt(result$count)
  result$cv <- result$se/result$mean
  result$total <- result$mean * result$area/std.area
  Res.names <- Names[result$strata]
  aggr.output <- data.frame(matrix(0, length(combine.output), 6))
  names(aggr.output) <- c("mean", "se", "cv", "count", "area", "total")
  row.names(aggr.output) <- names(combine.output)
  for (i in 1:length(combine.output)) {
    j <- !is.na(match(result$strata, combine.output[[i]]))
    j1 <- c(1:length(j))
    j1 <- j1[j]
    if (length(j1) > 0) aggr.output[i, ] <- Combine.strata(result, j1)
  }

  aggr.output$area <- round(aggr.output$area)
  aggr.output$mean <- round(aggr.output$mean, 3)
  aggr.output$se <- round(aggr.output$se, 3)
  aggr.output$cv <- round(aggr.output$cv, 3)
  aggr.output$se <- round(aggr.output$se, 4)
  aggr.output$total <- round(aggr.output$total, 1)
  result$area <- round(result$area)
  result$mean <- round(result$mean, 3)
  result$sdev <- round(result$sdev, 3)
  result$cv <- round(result$cv, 3)
  result$se <- round(result$se, 4)
  result$total <- round(result$total, 1)
  return(list(result = result, Res.names = Res.names, aggr.output = aggr.output))
}

#' Sums mean, standard error, cv by aggregated area
#'
#' Function used both in \code{\link{Calc.index}}.
#'
#' @export
#' @aliases index.aggregate.combine
#' @param result Result
#' @param index Index
Combine.strata <- function (result, index)
{
  if (!missing(index)) result <- result[index, ]
  Mean <- sum(result$mean * result$area)/sum(result$area)
  Sum <- sum(result$mean * result$area)
  total <- sum(result$total)
  totalarea <- sum(result$area)
  i <- !is.na(result$sdev)
  tmpsum <- sum(result$mean[i] * result$area[i])
  Calc.sdev <- sqrt(sum(result$sdev[i]^2 * result$area[i]^2/result$count[i])/sum(result$area[i])^2)
  Sdev <- Calc.sdev * Sum/tmpsum
  return(data.frame(mean = Mean, se = Sdev, cv = Sdev/Mean,
                    count = sum(result$count), area = totalarea, total = total))
}


