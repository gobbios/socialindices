#' DSI ("dyadic CSI")
#'
#' @param b.source data frame with behavioural data
#' @param from character or date in format YYYY-MM-DD, by default the earliest and latest date in \code{b.source}, respectively
#' @param to character or date in format YYYY-MM-DD, by default the earliest and latest date in \code{b.source}, respectively
#' @param behaviours character string with codes of behaviours to be considered, by default all behaviours that occur in the data are considered. Can also be a named list if merging of different codes into broader categories is desired (see details).
#' @param ot.source data frame with observation time data
#' @param ot.type eiter \code{"table"} (OT for each ID, separated by date) or \code{"party"}
#' @param duration.NA.treatm by default \code{"omit"}, i.e. if there is a duration column in \code{behav.source} and not all data points have a duration (i.e. NA) for a given behaviour (e.g. grooming), these NA data points will be deleted; alternatively \code{"count"}, i.e. behaviours for which not all durations are given will be transformed into "1", i.e. they will be counted as single occurences (for example, grooming will become "grooming bouts")
#' @param presence optional data.frame in the form of date by ID, with 0 and 1 indicating whether an ID was present on a given day or not; first column must be a date (YYYY-MM-DD); see \code{\link{createnullpresence}}
#' @param directed logical, if actors and receivers are differentiated, a directed CSI is returned, i.e. with each dyad represented twice (once in each direction within the dyad)
#' @param onlyfocaldyads logical, \code{TRUE} by default. Should only focal-focal dyads be returned? If \code{FAlSE}, returns also focal-nonfocal dyads if such were found in the data.
#' @param limit2focalnonfocal logical, \code{FALSE} by default. Limits returned data to focal-nonfocal dyads. This is relevant only if there are such dyads in the data set and if \code{onlyfocaldyads=FALSE}. Otherwise this argument is ignored and a warning returned.
#'
#' @details Typically, behaviours to be considered are supplied in the form \code{behaviours=c("groom", "approach")}. If you wish to merge behaviours into broader categories, supply a named list, e.g. \code{list(groom=c("grm.received", "grm.given"), greet=c("greet", "mount", "grunt"))}. In such a case, behaviours in a given category are summed first to calculate the rates with which the behaviour categories occur.
#' @return a data.frame with DSI and standardized DSI. For the latter, the rates of behaviours are z-transformed before they are summed and divided by the number of behaviours.
#' @importFrom stats na.omit
#' @export
#' @references Silk, J. B., Altmann, J., & Alberts, S. C. (2006). Social relationships among adult female baboons (Papio cynocephalus) I. Variation in the strength of social bonds.Behavioral Ecology and Sociobiology,61(2), 183–195. https://doi.org/10.1007/s00265-006-0249-2
#' Silk, J. B., Cheney, D. L., & Seyfarth, R. M. (2013). A practical guide to the study of social relationships. Evolutionary Anthropology,22(5), 213–225. https://doi.org/10.1002/evan.21367
#'
#' @examples
#' \dontrun{
#' data(dataset3)
#' DSI(dataset3$dataseq, presence = dataset3$presence)
#' DSI(dataset3$dataseq)
#'
#' data(dataset3)
#' DSI(dataset3$dataseq, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = FALSE) #default
#' DSI(dataset3$dataseq, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = FALSE)
#' DSI(dataset3$dataseq, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = TRUE)
#' DSI(dataset3$dataseq, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = TRUE)
#'
#' DSI(dataset3$dataseq, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = FALSE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = FALSE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = TRUE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = TRUE, presence=dataset3$presence)
#'
#' DSI(dataset3$dataseq, ot.source=dataset3$ot, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = FALSE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, ot.source=dataset3$ot, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = FALSE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, ot.source=dataset3$ot, onlyfocaldyads=FALSE,
#' limit2focalnonfocal = TRUE, presence=dataset3$presence)
#' DSI(dataset3$dataseq, ot.source=dataset3$ot, onlyfocaldyads=TRUE,
#' limit2focalnonfocal = TRUE, presence=dataset3$presence)
#' }

DSI <- function(b.source, from=NULL, to=NULL, behaviours=NULL, ot.source=NULL, ot.type="table", duration.NA.treatm="omit", presence=NULL, directed=FALSE, onlyfocaldyads=TRUE, limit2focalnonfocal = FALSE) {

  # limit objects to date range
  d <- varname("date", b.source);
  b.source[, d] <- as.Date(as.character(b.source[, d]))
  if(is.null(from)) { from <- min(as.Date(b.source[, d])) } else { from <- as.Date(from) }
  if(is.null(to))   { to   <- max(as.Date(b.source[, d])) } else { to   <- as.Date(to)   }
  b.source <- droplevels(b.source[b.source[, d] >= from & b.source[, d] <= to , ])
  rm(d)

  if(!is.null(ot.source)) {
    d <- varname("date", ot.source);
    ot.source[, d] <- as.Date(as.character(ot.source[, d]))
    ot.source <- droplevels(ot.source[ot.source[, d] >= from & ot.source[, d] <= to , ])
    rm(d)
  }
  if(!is.null(presence)) {
    d <- varname("date", presence);
    presence[, d] <- as.Date(as.character(presence[, d]))
    presence <- presence[presence[, d] >= from & presence[, d] <= to , ]
    rm(d)
  }

  # unify b.source (create my column names...)
  b.source <- .mergeactrec(b.source)

  # get focals and nonfocals
  # first from b.source
  focals <- sort(unique(b.source$XFOCAL))
  nonfocs <- sort(unique(b.source$XPARTNER))
  nonfocs <- nonfocs[!nonfocs %in% focals]

  # now check in OT
  if(!is.null(ot.source)) {
    fadd <- sort(unique(as.character(ot.source[, varname("focal", ot.source)])))
    focals <- sort(unique(c(focals, fadd)))
    if(length(nonfocs)>0) {
      nonfocs <- nonfocs[!nonfocs %in% focals]
    }
  }

  # and create OT if it doesnt exist yet (and set style to "OT")
  if(is.null(ot.source)) {
    ot.source <- createnullot(focals, from, to)
    ot.type <- "table"
  }


  # now get the dyads that we are interested in, depending on the 'onlyfocaldyads' and 'limit2focalnonfocal' arguments
  if(length(nonfocs)==0 & onlyfocaldyads==FALSE) {
    warning("no focal-nonfocal dyads found, assuming only focal-focal dyads", call.=FALSE)
    onlyfocaldyads <- TRUE
  }
  if(onlyfocaldyads) { dyads <- makedyads(focals) }

  if(!onlyfocaldyads) {
    dyads <- makedyads(c(focals, nonfocs))
    if(limit2focalnonfocal) {
      dyads <- dyads[dyads[, 1] %in% focals + dyads[,2] %in% focals == 1, ]
    }
  }
  if(length(nonfocs) > 1 ) {
    dyads <- dyads[!(dyads[, 1] %in% nonfocs + dyads[, 2] %in% nonfocs == 2), ]
  }


  # create presences and unify them...
  # perhaps go back to have only one presence supplied?
  if(is.null(presence)) {
    presence <- createnullpresence(sort(c(focals, nonfocs)), from, to)
  }
  #   if(is.null(presence2)) {
  #     presence2 <- createnullpresence(sort(c(focals, nonfocs)), from, to)
  #   }

  #presence <- cbind(presence, presence2)
  pres <- presence[, !duplicated(colnames(presence))]

  # check whether all IDs are in 'pres', and if not return error
  for(i in c(focals, nonfocs)) if(!i %in% colnames(pres)) stop("error 1: not all IDs found in presence data")

  # now calculate dyadic observation time...
  # premise here is that at least dyad member was a focal and therefore has an entry in the ot.source
  # if that's not the case, zero dot will be returned
  # 'res' will be the results backbone...
  res <- dot(ot.source, dyads, daterange=c(from, to), presence=presence, ot.style=ot.type)
  # res needs to be doubled if directionality is to be considered
  res <- rbind(res, dot(ot.source, dyads[, c(1,2,4,3)], c(from,to), presence =presence, ot.style=ot.type))
  res <- res[res$cores>0, ]
  # if( 0 %in% res$dot ) res <- res[res$dot > 0 , ]
  rownames(res) <- NULL
  # save(ot.source, dyads, from, to, presence, ot.type, file="dottesting.RData")

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

  beh <- droplevels(b.source[b.source[, BEH] %in% behaviours, ])

  # check whether there is a duration column, and if not create one and set all behaviours to "1", so they will effectively be treated as counts/bouts...
  if(is.na(varname("duration", beh))) beh$dur <- 1

  # clear all individuals from the data set that are not either focal or nonfocal


  # now loop through behaviours
  # first create a new vector for behaviour names with "rt" attached ("rate" per dot)
  newn <- c(); notusedbeh <- c()
  b=behaviours[1]
  for(b in behaviours) {
    temp <- beh[beh[, BEH]==b, ]

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
      # and finally loop through dyads
      i1 <- unlist(lapply(strsplit(as.character(res$dyad), "_@_"), function(x)x[1]))
      i2 <- unlist(lapply(strsplit(as.character(res$dyad), "_@_"), function(x)x[2]))

      dy=1
      for(dy in 1:nrow(res)) {
        te <- subset(temp, (temp$XPARTNER==i2[dy] & temp$XFOCAL==i1[dy] & temp$focwasact) | (temp$XPARTNER==i1[dy] & temp$XFOCAL==i2[dy] & !temp$focwasact))
        x <- sum(te[, dur])
        res[dy, b] <- x
        rm(x, te)
      }

    }

  }

  # and assign dyadtype
  dty <- as.character(apply(cbind(i1,i2), 1, function(x)sum(x %in% focals)))
  dty[dty==1] <- "FNF"; dty[dty==2] <- "FF"

  # add individual IDs for easier reference
  res <- data.frame(i1, i2, type=dty, res)

  # check whether one of the behaviours was never observed
  (bsums <- colSums(res[, behaviours, drop=FALSE]))

  if(0 %in% bsums) {
    ex <- names(bsums)[bsums==0]
    res <- res[, -c(which(colnames(res) %in% ex)), drop=FALSE]
    behaviours <- behaviours[-c(which(behaviours %in% ex))]
    newn <- newn[-c(which(unlist(strsplit(newn, ".rt")) %in% ex))]
    warning(paste(paste(ex, collapse=","), ": was/were excluded because the behaviour didnt occur in the data"), call.=FALSE)
  }

  # reformat if directionality is not required
  if(!directed) {
    adddata <- res[(nrow(res)/2+1):nrow(res), behaviours, drop=FALSE]
    res[1:(nrow(res)/2), behaviours] <- res[1:(nrow(res)/2), behaviours, drop=FALSE] + adddata
    res <- res[1:(nrow(res)/2), ]
    i1 <- i1[1:(nrow(res)/2)]; i2 <- i2[1:(nrow(res)/2)]
  }

  # check whether only focal dyads are requested but limit2... is set to TRUE
  if(onlyfocaldyads & limit2focalnonfocal) {
    limit2focalnonfocal <- FALSE
    warning("only focal dyads requested, but limited to foc-nonfoc dyads? Limit is ignored", call.=FALSE)
  }
  if(limit2focalnonfocal) {
    res <- res[ (i1 %in% focals + i2 %in% focals) == 1, ]
  }

  # deal with zero or NA dots (can happen because despite coresidency either no focal protocols for either or never observed in the same party)
  # in effect the result for such dyads is the same, a CSI/DSI is not defined because they never had the opportunity to interact (at least to be observed interacting...)
  odyads <- which(res$dot>0 & !is.na(res$dot) )

  # calculate rates etc
  res[, newn] <- NA
  res[odyads, newn] <- res[odyads, behaviours]/res$dot[odyads]
  res[odyads, newn] <- apply(res[odyads, newn, drop=FALSE], 2, function(x)x/mean(x))

  # and finally calculate standard DSI and based after z-transform
  res$DSI <- NA; res$zDSI <- NA
  res$DSI[odyads]  <- round(rowSums(      res[odyads , newn, drop=FALSE]) /length(behaviours), 5)
  # scaling gives error if all data points are identical (ie if SD=0), so exclude such variables if present
  if(0 %in% apply(res[odyads, newn, drop=FALSE], 2, sd)) newn <- newn[-c(which(apply(res[odyads, newn, drop=FALSE], 2, sd)==0))]
  res$zDSI[odyads] <- round(rowSums(scale(res[odyads , newn, drop=FALSE]))/length(newn), 5)

  # just check per definition, mean of all DSIs should be one (or zero if z-transformation was applied)
  mean(res$DSI[odyads]); mean(res$zDSI[odyads])
  if(round(mean(res$DSI[odyads]) , 2)!=1 ) warning("avg. DSI not exactly one (MESSAGE 9)", call.=FALSE)
  if(round(mean(res$zDSI[odyads]), 2)!=0 ) warning("avg. z-DSI not exactly zero (MESSAGE 10)", call.=FALSE)

  # a final check whether NA's appear anywhere in the results..
  if(nrow(res[odyads, ])!=nrow(na.omit(res[odyads, ]))) warning("some NAs somewhere (MESSAGE 11)")

  # and give message if dyads had 0 or NA dot, and hence NA DSI
  if(length(odyads) < nrow(res)) message("some dyads don't have defined CSI/DSI despite co-residency")

  return(res)

}
