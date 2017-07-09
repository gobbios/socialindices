#' CSI (individual CSI)
#'
#' @param b.source data frame with behavioural data
#' @param from character or date in format YYYY-MM-DD, by default the earliest and latest date in \code{b.source}, respectively
#' @param to character or date in format YYYY-MM-DD, by default the earliest and latest date in \code{b.source}, respectively
#' @param behaviours character string with codes of behaviours to be considered, by default all behaviours that occur in the data are considered. Can also be a named list if merging of different codes into broader categories is desired (see details).
#' @param ot.source data frame with observation time data
#' @param ot.type character: \code{"table"} (OT for each ID, separated by date). "party"-style not supported!
#' @param duration.NA.treatm by default \code{"omit"}, i.e. if there is a duration column in \code{behav.source} and not all data points have a duration (i.e. NA) for a given behaviour (e.g. grooming), these NA data points will be deleted; alternatively \code{"count"}, i.e. behaviours for which not all durations are given will be transformed into "1", i.e. they will be counted as single occurences (for example, grooming will become "grooming bouts")
#'
#' @details Typically, behaviours to be considered are supplied in the form \code{behaviours=c("groom", "approach")}. If you wish to merge behaviours into broader categories, supply a named list, e.g. \code{list(groom=c("grm.received", "grm.given"), greet=c("greet", "mount", "grunt"))}. In such a case, behaviours in a given category are summed first to calculate the rates with which the behaviour categories occur.
#'
#' @importFrom stats median sd
#' @return a data.frame with CSI and standardized CSI. For the latter, the rates of behaviours are z-transformed before they are summed and divided by the number of behaviours.
#' @export
#' @references Sapolsky, R. M., Alberts, S. C., & Altmann, J. (1997). Hypercortisolism associated with social subordinanceor social isolation among wild baboons.Archives of General Psychiatry,54(12), 1137–1143. \url{https://doi.org/10.1001/archpsyc.1997.01830240097014}
#' @examples
#' \dontrun{
#' data(dataset3)
#' CSI(dataset3$dataseq)
#' }

CSI <- function(b.source, from=NULL, to=NULL, behaviours=NULL, ot.source=NULL, ot.type="table", duration.NA.treatm="omit") {
  if(is.null(from) & is.null(to)) {
    from <- min(as.Date(as.character(b.source[, varname("date", b.source)])))
    to   <- max(as.Date(as.character(b.source[, varname("date", b.source)])))
  }

  b.source <- droplevels(limit2daterange(b.source, c(from, to)))

  # and create OT if it doesnt exist yet (and set style to "table")
  if(is.null(ot.source)) {
    focs <- sort(unique(as.character((b.source[, varname("focal", b.source)]))))
    ot.source <- createnullot(focs, from, to)
    ot.type <- "table"
  }

  ot.source <- droplevels(limit2daterange(ot.source, c(from, to)))

  focs <- sort(unique(as.character((ot.source[, varname("focal", ot.source)]))))

  # find behaviours selected or (by default) all that occur in the data
  BEH <- varname("behaviour", b.source)
  if(is.null(behaviours)) behaviours <- sort(unique(as.character(b.source[, BEH])))
  # and merge behaviours in case a list was supplied
  if(is.list(behaviours)) {
    if(is.null(names(behaviours))) {
      bnames <- paste("B", 1:length(behaviours), sep=".")
    } else {
      bnames <- names(behaviours)
      if("" %in% bnames) {
        bnames <- paste("B", 1:length(behaviours), sep=".")
        warning("not all behaviours were named in the list", call.=FALSE)
      }
    }
    b.source[, BEH] <- as.character(b.source[, BEH])
    for(i in 1:length(behaviours)) {
      b.source[b.source[, BEH] %in% behaviours[[i]], BEH] <- bnames[i]
    }
    behaviours <- bnames
  }

  beh <-  .mergeactrec(droplevels(b.source[b.source[, BEH] %in% behaviours, ]))

  # check whether there is a duration column, and if not create one and set all behaviours to "1", so they will effectively be treated as counts/bouts...
  if(is.na(varname("duration", beh))) beh$dur <- 1

  # make results table
  ot <- as.numeric(tapply(ot.source[, varname("obstime", ot.source)], as.character(ot.source[, varname("focal", ot.source)]), sum))
  res <- data.frame(focal=focs, ot=ot); rm(ot)

  # now loop through behaviours
  # first create a new vector for behaviour names with "rt" attached ("rate" per dot)
  newn <- c(); notusedbeh <- c()
  b=behaviours[1]
  for(b in behaviours) {
    temp <- beh[beh[, BEH]==b & beh[, "focwasact"], ]

    # if behaviour didnt occur
    if(nrow(temp)==0) {
      res[, b] <- 0
      n <- paste(b, "rt", sep=".")
      newn <- c(newn, n); rm(n)
    }

    if(nrow(temp)>0) {
      res[, b] <- NA
      n <- paste(b, "rt", sep=".")
      newn <- c(newn, n); rm(n)

      # check whether there is a duration column and if there is not, create one with all durations set to "1", i.e. make them counts
      # also remove NAs if there is actually a duration column but "omit" has been specified, or transfer data into counts ("count")
      dur <- varname("duration", temp)
      if(sum(is.na(temp[, dur])) == nrow(temp)) temp[, dur] <- 1
      if(sum(is.na(temp[, dur])) != nrow(temp) & sum(is.na(temp[, dur])) != 0 ) {
        if(duration.NA.treatm=="omit") temp <- temp[!is.na(temp[, dur]), ]
        if(duration.NA.treatm=="count") temp[, dur] <- 1
      }
      # loop through individuals
      f=focs[1]
      for(f in focs) {
        te <- temp[temp[, "XFOCAL"]==f, ]
        res[res$focal==f, b] <- sum(te[, dur])
        rm(te)
      }
      rm(f)
    }

  }
  # check whether one of the behaviours was never observed
  (bsums <- colSums(res[, behaviours, drop=FALSE]))

  if(0 %in% bsums) {
    ex <- names(bsums)[bsums==0]
    res <- res[, -c(which(colnames(res) %in% ex))]
    behaviours <- behaviours[-c(which(behaviours %in% ex))]
    newn <- newn[-c(which(unlist(strsplit(newn, ".rt")) %in% ex))]
    warning(paste(paste(ex, collapse=","), ": was/were excluded because the behaviour didnt occur in the data"), call.=FALSE)
  }

  # rates and standardize values (by MEDIAN, cf Sapolsky et al 1997!!!)
  res[, newn] <- res[, behaviours, drop=FALSE]/res$ot
  res[, newn] <- apply(res[, newn, drop=FALSE], 2, function(x)x/median(x))
  # and finally calculate standard CSI and based after z-transform
  res$CSI  <- round(rowSums(      res[ , newn, drop=FALSE]) /length(behaviours), 5)
  # scaling gives error if all data points are identical (ie if SD=0), so exclude such variables if present
  if(0 %in% apply(res[, newn, drop=FALSE], 2, sd)) newn <- newn[-c(which(apply(res[, newn], 2, sd)==0))]
  res$zCSI <- round(rowSums(scale(res[ , newn, drop=FALSE]))/length(newn), 5)


  return(res)

}
