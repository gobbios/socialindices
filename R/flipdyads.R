#' flip dyads in (0,1) table
#'
#' flip dyads in 0/1 matrix to randomize data set
#'
#' @param x data frame with association data
#' @param x.dates Date, dates corresponding to rows in \code{x} (\code{x.dates=}), i.e. of same length as rows of \code{x}, and corresponding to \code{presence} (\code{pr.dates=})
#' @param flips numeric, how many flips to perform
#' @param presence presence matrix; see \code{\link{createnullpresence}}
#' @param pr.dates Date, dates corresponding to rows in \code{x} (\code{x.dates=}), i.e. of same length as rows of \code{x}, and corresponding to \code{presence} (\code{pr.dates=})
#' @param x.fo character string corresponding to rows in \code{x} with focal animal IDs, if supplied, focal animals are excluded from flipping, i.e. focals remain in their party/association
#'
#' @return a table with the same dimensions as \code{x}, except that the date column is removed if there was one in \code{x} when it was supplied to the function.
#' @references
#' Whitehead 2008; Bejder et al 1998
#'
#' @examples
#' \dontrun{
#' data(dolphins)
#' # create a presence table (all being present all the time)
#' dpres <- createnullpresence(colnames(x)[2:19], from="2000-01-01", "2000-02-09")
#' # without presence
#' colSums(flipdyads(dolphins, 10))
#' # with presence
#' colSums(flipdyads(dolphins, 10, dolphins$Date, presence))
#'
#' dolphins[,-1] - flipdyads(dolphins, 10)
#' sum(abs(dolphins[,-1] - flipdyads(dolphins, 20)))
#' }

.flipdyads <- function(x, x.dates, flips, presence=NULL, pr.dates=NULL, x.fo=NULL) {
  # first determine whether presence is there and determine mode accordingly
  if(is.null(presence)) mode <- "nopres"

  if(!is.null(presence)) {
    if(is.null(pr.dates)) stop("dates vector has to be supplied for presence", call.=FALSE)
    if (!inherits(presence, "matrix")) stop("presence has to be of class matrix!", call.=FALSE)
    mode <- "withpres"
    pr.dates <- as.character(pr.dates)
  }

  if(length(x.dates)!=nrow(x)) stop("x.dates vector has to be of same length as 'x' has rows", call.=FALSE)
  x.dates <- as.character(x.dates)
  if (!inherits(x, "matrix")) stop("x has to be of class matrix!", call.=FALSE)


  # exclude rows in which all ids were together (regardless of focal remove or not...)
  xrows <- 1:nrow(x)
  ex <- which(rowSums(x)==ncol(x))
  if(length(ex)>0) xrows <- xrows[-ex]
  rm(ex)

  # replace the focals WITHIN a party/association with NA, so that they won't be flipped
  if(!is.null(x.fo)) {
    focols <- as.numeric(sapply(x.fo, function(X) which(colnames(x)==X), simplify = TRUE))
    x[cbind(1:nrow(x), focols)] <- NA
  }


  if(mode=="withpres") {
    repl <- c(0,1,1,0)
    ids <- colnames(x)
    SWITCH <- 0
    while(SWITCH < flips) {
      (xro1 <- sample(xrows, 1))
      xco1 <- which(x[xro1, ]==1)
      if(length(xco1)>1) xco1 <- sample(xco1, 1)
      xco2 <- sample(which(x[xro1, ]==0), 1)
      xro2 <- intersect(which(x[,xco1]==0), which(x[, xco2]==1))
      if(length(xro2)>1) xro2 <- sample(xro2, 1)
      if(length(xro2)==1) {
        if(cores(ids[xco1], ids[xco2], x.dates[c(xro1,xro2)], presence, "2days", pr.dates=pr.dates)) {
          x[c(xro2, xro1) , c(xco2, xco1)] <- repl
          SWITCH <- SWITCH + 1
        }
      }
      rm(xro1, xro2, xco1, xco2)
    }

  }

  if(mode=="nopres") {
    repl <- c(0,1,1,0)
    SWITCH <- 0
    while(SWITCH < flips) {
      (xro1 <- sample(xrows, 1));
      (xco1 <- which(x[xro1, ]==1))
      if(length(xco1)>1) (xco1 <- sample(xco1, 1))
      xco2 <- sample(which(x[xro1, ]==0), 1)
      xro2 <- intersect(which(x[,xco1]==0), which(x[, xco2]==1))
      (if(length(xro2)>1) xro2 <- sample(xro2, 1))
      if(length(xro2)==1) {
        x[c(xro2, xro1) , c(xco2, xco1)] <- repl
        SWITCH <- SWITCH + 1
      }; #nrow(x)
      rm(xro1, xro2, xco1, xco2)
    }

  }

  if(!is.null(x.fo)) {
    # and put the focals back in
    x[cbind(1:nrow(x), focols)] <- 1
  }

  return(x)

}

