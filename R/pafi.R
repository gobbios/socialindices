#' Pair-wise affinity index
#'
#' @param assodata data frame with association data. Can include a date column (ignored if no daterange is specified), and a focal column (if not found, column names (excluding date if found) are assumed to be "focals", i.e. individuals that are considered for calculating the index).
#' @param presence optional data.frame in the form of date by ID, with 0 and 1 indicating whether an ID was present on a given day or not; first column must be a date (YYYY-MM-DD); see \code{\link{createnullpresence}}
#' @param daterange character or date of length 2 (format YYYY-MM-DD). Ignored, if no date column is found. For \code{multiasso()} it can also be a list with pairs of dates or one of the following single characters: \code{"months"}, \code{"bimonth"} or \code{"trimonth"}, (details below)
#' @param flips the number of iterations WITHIN each randomization, i.e. how many dyads are swapped
#' @param rand numeric. Number of times, a null association data set is created while keeping distribution of association (party) sizes constant and the number of times each individual appears. Used to assess an expected association index (which can be used to test null hypothesis that observed associations are random).
#' @param removesolitary logical, by default \code{FALSE}. should parties/associations that consist of only one individual (the focal if focal column is found) be excluded.
#' @param addIDs logical, by default \code{FALSE}. should individuals that are not focals (if there is a focal column in \code{assodata}), but represented by columns be included in dyads to be calculated. E.g. if there is a focal column, and \code{addIDs=TRUE} than all focal-nonfocal dyads will be considered (in addition to focal-focal dyads), while nonfocal-nofocal dyads will be ignored. Remove the focal column if you whish to use all dyads that are represented by columns (or add the name of the focal column in the \code{exclcols=} argument).
#' @param exclcols character string with column names (or numerical index of these) to be excluded. Normally used to exclude columns that neither represent IDs, date or focal.
#' @param progbar logical, should a progress bar be displayed? by default \code{TRUE}
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @references
#' Pepper et al 1999
#' @details
#' Note that the algorithm for the randomization is not very efficient, i.e. it is slow. So use a small \code{rand} to verify that the function indeed works as expected before using a ``proper'' value (>=1000)...
#'
#' Future versions should include the possibility to differentiate between focals and other IDs, as well as a function to create artificial association data sets for testing purposes... Another thing to do is to allow the possibility to integrate neighbor data, which is essentially the same idea but could contain "parallel" data sets according to different distance categories.
#'
#' Also, as of now, possible problems include cases in which a single association/party includes \emph{all} individuals, i.e. this might cause a problem for the randomization test, at least in \code{HWI()}.
#'
#' To be done as well is to make sure that if there is a focal column, that the entry for the respective focal individual is set to 1, ie. to indicate presence in its own party (done in v0.34). More generally, make sure to consider the difference between party data and neighbor data!
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data(dolphins)
#' dpr <-  createnullpresence(colnames(dolphins)[2:19], from="2000-01-01", "2000-02-09")
#' # pairwise affinity index
#' pafi(dolphins, flips=100, rand=100)
#' pafi(dolphins, flips=100, rand=100, presence = dpr)
#' }

pafi <- function(assodata, presence=NULL, daterange=NULL, flips=500, rand=10, removesolitary=FALSE, addIDs=FALSE, exclcols=NULL, progbar=TRUE) {
  # pairwise affinity index (cf. Pepper et al 1999)
  #HWI2 <- function(assodata,  presence=NULL, daterange=NULL, flips=NULL, rand=0, exclcols=NULL, progbar=TRUE, addIDs=FALSE)
  # handling of data
  {
    # outlines for first (handling) part
    # 1 for assodata: check if there is date, if yes, vectorize it and remove from DF, do the same for focal, remove exclcols, convert to matrix
    # 2 do the same for presence (except focal and exclcols)
    # 3 adjust date range
    # 4 which dyads to be considerd
    # 5 run some checks
    # 6 prepare results (dyads, coresidency etc...)

    # step 1 - assodata

    if(!is.null(exclcols)) {
      exclcols <- which(colnames(assodata) %in% exclcols)
      assodata <- assodata[, -exclcols]
    }

    dte.asso.n <- varname("date", assodata, TRUE)
    if(!is.na(dte.asso.n)) {
      dte.asso <- as.Date(as.character(assodata[, dte.asso.n]))
      assodata <- assodata[, -c(which(colnames(assodata)==dte.asso.n))]
      rm(dte.asso.n)
    } else {
      dte.asso <- NULL; daterange <- NULL; presence <- NULL
      message("no date column found, date range and/or presence is ignored")
    }

    foc.asso.n <- varname("focal", assodata, TRUE)
    if(!is.na(foc.asso.n)) {
      foc.asso <- as.character(assodata[, foc.asso.n])
      assodata <- assodata[, -c(which(colnames(assodata)==foc.asso.n))]
      rm(foc.asso.n)
    } else {
      foc.asso <- NULL
    }

    assodata <- as.matrix(assodata)

    # step 2 - presence
    if(!is.null(presence)) {
      dte.pres.n <- varname("date", presence, TRUE)
      if(!is.na(dte.pres.n)) {
        dte.pres <- as.Date(as.character(presence[, dte.pres.n]))
        presence <- presence[, -c(which(colnames(presence)==dte.pres.n))]
        rm(dte.pres.n)
      } else {
        dte.pres <- NULL
      }

      presence <- as.matrix(presence)
    } else {
      dte.pres <- NULL
    }


    # step 3 - limit to date range
    if(is.null(daterange)) {
      if(!is.null(dte.asso)) {
        daterange <- range(dte.asso)
      } else {
        message("no dates found in 'assodata', presence ignored")
      }
    }


    if(!is.null(daterange)) {
      daterange <- as.Date(daterange)
      if(!is.null(dte.asso)) {
        assodata <- assodata[dte.asso>=daterange[1] & dte.asso<=daterange[2], ]
        dte.asso <- dte.asso[dte.asso>=daterange[1] & dte.asso<=daterange[2]]
      }
      if(!is.null(dte.pres)) {
        presence <- presence[dte.pres>=daterange[1] & dte.pres<=daterange[2], ]
        dte.pres <- dte.pres[dte.pres>=daterange[1] & dte.pres<=daterange[2]]
      }
    }

    # step 4 - which dyads
    focs <- NULL
    # first primary (i.e. "focals"), with descending priority to:
    # (1) designated focals in assodata
    if(!is.null(foc.asso)) focs <- sort(unique(foc.asso))
    # (2) colnames of assodata (if no focal is specified)
    if(is.null(focs)) focs <- sort(colnames(assodata))
    # and limit to those that also occur in presence data
    if(!is.null(presence)) {
      focs <- intersect(focs, colnames(presence))
    }
    addid <- NULL
    # and then, secondary (additional IDs), dyads between which will not be considered
    if(!is.null(foc.asso) & addIDs) {
      addid <- colnames(assodata)[!colnames(assodata) %in% focs]
      if(!is.null(presence)) addid <- addid[addid %in% colnames(presence)]
    }

    # and then check again whether all appear in 'presence'
    if(!is.null(presence)) {
      if(length(intersect(focs, colnames(presence)))!=length(focs)) {
        presence <- NULL
        message("not all focal IDs found in presence -> presence ignored")
      }
      if(length(intersect(addid, colnames(presence)))!=length(addid)) {
        presence <- NULL
        message("not all additional IDs found in presence -> presence ignored")
      }
    }
    # and set presence to contain only focals and additional
    if(!is.null(presence)) {
      presence <- presence[, colnames(presence) %in% c(focs, addid)]
    }
    # and the same for assodata
    assodata <- assodata[, colnames(assodata) %in% c(focs, addid)]

    # and make sure focals have a "1"
    if(!is.null(foc.asso)) {
      cols <- as.numeric(sapply(foc.asso, function(x) which(colnames(assodata)==x), simplify = TRUE))
      assodata[cbind(1:nrow(assodata), cols)] <- 1
    }

    # remove associations of one individual, if desired...
    if(removesolitary) {
      excl <- as.numeric(which(rowSums(assodata)==1))
      if(length(excl)>0) {
        assodata <- assodata[-excl, ]
        if(!is.null(dte.asso)) dte.asso <- dte.asso[-excl]
        if(!is.null(foc.asso)) foc.asso <- foc.asso[-excl]
      }
    }
    # and remove associations that contain no individuals (because of selections steps before)
    excl <- as.numeric(which(rowSums(assodata)==0))
    if(length(excl)>0) {
      assodata <- assodata[-excl, ]
      if(!is.null(dte.asso)) dte.asso <- dte.asso[-excl]
      if(!is.null(foc.asso)) foc.asso <- foc.asso[-excl]
    }; rm(excl)


    # step 5 - some checks?
    # e.g. do the date columns overlap sufficiently?

    # step 6 - results prep
    # create results backbone, i.e. decide which dyads
    res <- t(combn(c(focs, addid), 2))
    res <- res[which(apply(res, 1, function(x)sum(x %in% focs)>=1)), ]

    # coresidency
    coresid <- NULL; coresobs <- NULL
    if(!is.null(presence)) coresid <- apply(res, 1, function(x) sum(rowSums(presence[, x])==2) )
    if(is.null(coresid)) coresid <- rep(1, nrow(res))
    res <- res[coresid>0, ]
    if(nrow(res)<length(coresid)) message(paste(length(coresid)-nrow(res), "dyads excluded because they were never co-resident"))
    coresid <- coresid[coresid>0]
    # check whether there were actual observations during coresidency days for each dyad...
    if(!is.null(presence)) {
      temppresence <- presence[dte.pres %in% dte.asso, ]
      coresobs <- apply(res, 1, function(X) sum(rowSums(temppresence[, X])==2))
      if(0 %in% coresobs) {
        res <- res[coresobs > 0, ]
        coresid <- coresid[coresobs > 0 ]
        message(paste(sum(coresobs==0), "dyads excluded because despite coresidency no observations during these periods"))
      }
    }


    ID1 <- as.character(res[,1]); ID2 <- as.character(res[,2])
  } # end handling part
  # now existing objects: focs, addid, daterange,


  # party sizes (s) (so they don't have to be recalculated within the loops...)
  ps <- function(x) apply(x, 1, function(z)sum(z==1))
  s <- as.numeric(ps(assodata))
  s1 <- s-1; s2 <- s*(s-1)

  if(is.null(flips)) flips <- nrow(assodata)/2

  righttail <- lefttail <- numeric(nrow(res)) #obs.both <- obs.1 <- obs.2 <-
  abovechance <- p.right <- p.left <- exp <- rep(NA, nrow(res))

  # function to select only lines for which a dayd was coresident
  f1 <- function(X) which(dte.asso %in% dte.pres[rowSums(presence[, X])==2])
  #x <- assodata
  foopres <- function(x) {
    # number of times a pair was observed in the same association (party, etc...) GIVEN CORESIDENCY
    IAB <- apply(res, 1, function(X)sum(rowSums(x[f1(X), X, drop=FALSE])==2))
    # number of A's and B's total sightings
    A <- apply(res, 1, function(y) sum(x[f1(y), y[1]] * s1[f1(y)]) )
    B <- apply(res, 1, function(y) sum(x[f1(y), y[2]] * s1[f1(y)]) )
    H <- apply(res, 1, function(X) sum(s2[f1(X)]))
    return((H*IAB) / (A*B))
  }

  foonopres <- function(x) {
    # number of times a pair was observed in the same association (party, etc...)
    IAB <- apply(res, 1, function(X)sum(rowSums(x[, X])==2)) # IAB
    # number of A's and B's total sightings
    A <- apply(res, 1, function(y) sum(x[, y[1]] * s1) )
    B <- apply(res, 1, function(y) sum(x[, y[2]] * s1) )
    return((sum(s2)*IAB) / (A*B))
  }


  if(is.null(presence)) {
    singelpafi <- foonopres(assodata)
    obs.both <- apply(res, 1, function(xy)sum(rowSums(assodata[, xy])==2)) # IAB
    obs.1 <- apply(res, 1, function(y) sum(assodata[, y[1]] * s1) )
    obs.2 <- apply(res, 1, function(y) sum(assodata[, y[2]] * s1) )

  } else {
    singelpafi <- foopres(assodata)
    obs.both <- apply(res, 1, function(X)sum(rowSums(assodata[f1(X), X, drop=FALSE])==2))
    obs.1 <- apply(res, 1, function(y) sum(assodata[f1(y), y[1], drop=FALSE] ) )
    obs.2 <- apply(res, 1, function(y) sum(assodata[f1(y), y[2], drop=FALSE] ) )
    #obs.1 <- apply(res, 1, function(y) sum(assodata[f1(y), y[1], drop=FALSE] * s1[f1(y)]) )
    #obs.2 <- apply(res, 1, function(y) sum(assodata[f1(y), y[2], drop=FALSE] * s1[f1(y)]) )
  }

  singelpafi[is.na(singelpafi)] <- 0

  # and run the randomization test for PAfI
  if(rand > 0) {
    if(progbar) pb <- txtProgressBar(0, rand, 0, style=3)

    randores <- matrix(ncol=rand, nrow=nrow(res))
    r=1
    if(is.null(presence)) {
      if(is.null(dte.asso)) dte.asso <- rep(1, nrow(assodata))
      for(r in 1:rand) {
        temp <- .flipdyads(assodata, dte.asso,  flips)
        randores[, r] <- foonopres(temp); rm(temp)
        if(progbar) setTxtProgressBar(pb, r)
      }
    } else {
      #rr <- sapply(1:rand, function(X) foopres(flipdyads(assodata, dte.asso, flips, presence, dte.pres)))
      #rr2 <- vapply(1:rand, function(X) foopres(flipdyads(assodata, dte.asso, flips, presence, dte.pres)), numeric(length(ID1)))
      for(r in 1:rand) {
        temp <- .flipdyads(assodata, dte.asso, flips, presence, dte.pres)
        randores[, r] <- foopres(temp); rm(temp)
        if(progbar) setTxtProgressBar(pb, r)
      }
    }
    if(progbar) close(pb)

    randores[is.na(randores)] <- 0

    # expected and p-values...
    exp <- rowMeans(randores)
    tempres <- cbind(singelpafi, randores)
    # observed larger or equal than original
    p.right <- rowSums(t(apply(tempres, 1, function(x)x[1]<=x[2:(rand+1)])))/rand
    # observed smaller or equal than original
    p.left <- rowSums(t(apply(tempres, 1, function(x)x[1]>=x[2:(rand+1)])))/rand

    # observed AI above or below expected...
    abovechance[p.right <= 0.05] <- TRUE; abovechance[p.left <= 0.05] <- FALSE

  }

  res <- data.frame(ID1, ID2, dyad=paste(ID1, ID2, sep="_@_"), coresid, obs.both, obs.1, obs.2, PAfI=singelpafi, exp, p.right, p.left, abovechance)

  return(res)
}


