#' generate random data for bond package
#'
#' @param bondsdata output from \code{\link{datagen}}
#' @param no number of interactions to be created
#' @param ago character, determines behaviours for the "agonistic" interactions of \code{domgen}
#' @param agow numeric, weights of occurences of behaviours in \code{domgen}
#' @param drawprop proportion of interactions that end undecided
#' @param reversals proportion of interactions that end against the implicit rule regarding which individual wins. By default this is alphabetic, i.e. of two individuals interacting, the winner precedes the loser in alphabetic order
#'
#' @details \code{domgen()} does not yet work by itself, i.e. it relies on specifying \code{bondsdata} being the output from \code{\link{datagen}}.
#' @return a data.frame with winner/loser interactions
#' @export
#'
#' @examples
#' x <- datagen(no=20, ni=3)
#' # observation time table
#' x$ot
#' # interactions
#' x$dataseq
#'
#' # and for dominance interactions
#' y <- domgen(x)

domgen <- function(bondsdata = NULL, no = 100, ago = c("suppl", "thrt", "fight"), agow = c(0.6, 0.3, 0.1), drawprop = 0.1, reversals = 0.1) {
  # take info from bondsdata if supplied
  if(!is.null(bondsdata)) {
    pmat <- bondsdata$presence
    #if(!is.null(bondsdata$presence2)) pmat <- cbind(pmat, bondsdata$presence2[, -1])
    allids <- colnames(pmat)[2:ncol(pmat)]
    ot <- bondsdata$ot

  } else {
    stop("not yet working if bondsdata is omitted...", call.=FALSE)
    ni=10
  }

  # create results table (along with specified draw proportion and types of interactions)
  res <- data.frame(Date=NA, datetime=NA, winner=factor(no, levels=allids), loser=factor(no, allids), type=sample(ago, no, TRUE, agow), draw=sample(c(TRUE, FALSE), no, TRUE, c(drawprop, 1-drawprop)))

  while(sum(!is.na(res$winner))<no) {
    # get index for the line
    loc <- min(which(is.na(res$winner)))

    # select random protocol with associated date and focal individual
    (prot <- ot[sample(1:nrow(ot), 1, prob=ot$OT), ])
    foc <- as.character(prot$focal); d <- prot$date

    # get possible partners on that date
    part <- pmat[pmat$date==d, -c(which(colnames(pmat) %in% c("date", foc)))]
    part <- names(part)[part==1]

    # only continue if there is at least one possible partner
    if(length(part)>=1) {
      res$Date[loc] <- as.character(d)
      res[loc, c("winner", "loser")] <- sort(c(foc, sample(part, 1)))
    }
  }

  # now add reversals
  if(round(no*reversals) >= 1) {
    locs <- sample(1:nrow(res), round(no*reversals))
    res[locs, c("winner", "loser")] <- res[locs, c("loser", "winner")]
  }

  # and finally create some random time (could lead to interaction sequence that does not correpsond to protocol sequence [the latter does not yet account for time...])
  res$datetime <- as.POSIXlt(paste(res$Date, paste(round(runif(no, 6, 17)), round(runif(no, 0, 59)), round(runif(no, 0, 59)), sep=":")), format="%Y-%m-%d %H:%M:%S")
  res$Date <- as.Date(res$Date)

  # and sort results accordingly
  res <- res[order(res$datetime), ]; rownames(res) <- NULL
  return(res)
}
