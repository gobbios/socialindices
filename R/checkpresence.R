#' Title
#'
#' @param presence presence "matrix" (usually a data frame...); see \code{\link{createnullpresence}}. \code{presence2} is only necessary if focal-nonfocal dyads are to be considered at the same time, e.g. when interested in male-male focal dyads and male-female focal-nonfocal dyads. In this case, \code{presence2} would be the female presence data.
#' @param b.source data frame with behavioural data
#' @param ot.source data frame with observation time
#'
#' @return a matrix, with dates and roles (actor, etc...) at which presence and behavioural data don't match. Same for observation time data. In case both are specified, a list is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataset3)
#' checkpresence(dataset3$pres, dataset3$dataseq)
#' checkpresence(dataset3$pres, dataset3$dataseq, dataset3$ot)
#'
#' x <- datagen(no=100, ni=7, presence=c(0.8,0.4))
#' checkpresence(x$pres, x$dataseq, x$ot)
#' }

checkpresence <- function(presence, b.source=NULL, ot.source=NULL) {
  Dpr <- varname("date", presence)
  presence[, Dpr] <- as.Date(as.character(presence[, Dpr]))

  if(!is.null(ot.source)) {
    Do <- varname("date", ot.source)
    ot.source[, Do] <- as.Date(as.character(ot.source[, Do]))
    Df <- varname("focal", ot.source)

    if(sum(!as.character(b.source[, Df]) %in% colnames(presence)) > 0) stop("issue 2 - focals in ot.source not appearing in presence", call.=F)

    # check whether presence covers the entire date range of b.source
    if(!(min(ot.source[, Do]) >= min(presence[, Dpr]) & max(ot.source[, Do]) <= max(presence[, Dpr]))) {
      if(!min(ot.source[, Do]) >= min(presence[, Dpr])) stop("date range of observation time starts before presence data", call. = F)
      if(!max(ot.source[, Do]) <= max(presence[, Dpr])) stop("date range of observation time goes beyond presence data", call. = F)
    }

    res <- matrix(ncol=3, nrow=0, "")
    i = 1
    for(i in 1:nrow(ot.source)) {
      if(presence[presence[, Dpr]==ot.source[i,Do], as.character(ot.source[i,Df])]==0) {
        res <- rbind(res, c(as.character(ot.source[i,Do]), as.character(ot.source[i,Df]), i))
      }
    }
    colnames(res) <- c("Date", "focal", "INDEX")
    if(length(res)==0) res <- "all good it seems (ot.source)"
    ot.check <- res
  }



  if(!is.null(b.source)) {
    Db <- varname("date", b.source)
    b.source[, Db] <- as.Date(as.character(b.source[, Db]))

    Df <- varname("focal", b.source)
    temp <- varname("IDs", b.source)

    i=3
    ch <- 0
    for(i in 1:length(temp)) {
      ch <- ch + sum(!as.character(b.source[, temp[i]]) %in% colnames(presence))
    }
    if(ch>0) stop("issue 1 - individuals in b.source not appearing in presence", call.=FALSE)
    #   colnames(presence)[!colnames(presence) %in% b.source[,temp[3]]]
    #   levels(b.source[, temp[3]])[!levels(b.source[, temp[3]]) %in% colnames(presence)]
    #
    res <- matrix(ncol=4, nrow=0,"")

    i=5
    for(i in 1:nrow(b.source)) {
      d <- b.source[i, Db]
      ids <- apply(b.source[i, temp], 1, as.character)
      ch <- 1 - presence[presence[, Dpr]==d, ids]
      if(sum(ch) > 0) {
        te <- temp[which(ch!=0)]
        for(m in which(ch!=0)) res <- rbind(res, c(as.character(d), temp[m], ids[m], i))
      }
    }


    colnames(res) <- c("Date", "role", "ID", "INDEX")
    b.check <- res

    if(length(b.check)==0) b.check <- "all good it seems (b.source)"


  }

  if(is.null(b.source) ) {return(ot.check)}
  if(is.null(ot.source) ) {return(b.check)}
  if(!is.null(ot.source) & !is.null(b.source)) {return(list(ot.check, b.check))}
}



