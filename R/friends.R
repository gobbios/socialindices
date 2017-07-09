#' top partners
#'
#' @param x output from \code{\link{DSI}}
#' @param criterion either one of \code{"1sd"}, \code{"2sd"}, \code{"10perc"}, \code{"5perc"}, or numeric
#' @param indexcols character, the column that contains the index of interest
#' @param IDcols character of length 2, column names of the two dyad members
#' @param tableout logical by default \code{FALSE}, if \code{TRUE} and a group level criterion was selected (not numeric), a table with the dyads in descending order of their dyadic index is returned
#'
#' @details if the criterion is one of \code{"1sd"}, \code{"2sd"}, \code{"10perc"}, \code{"5perc"} the cutoff is considered across all dyads in the data set. If the criterion is numeric, then the x top partners of each individual will be returned regardless of absolute values of the index. If \code{criterion=NULL}, its default, all possible dyads will be returned sorted by size within individual.
#'
#' @importFrom stats quantile
#' @return a list
#' @export
#'
#' @examples
#' data(dataset3)
#' dsi <- DSI(dataset3$dataseq, presence=dataset3$presence, ot.source=dataset3$ot,
#' onlyfocaldyads = FALSE)
#' friends(dsi, "2sd")
#' friends(dsi, "2sd", tableout = TRUE)
#' friends(dsi, 3)


friends <- function(x, criterion=NULL, indexcols="DSI", IDcols=c("i1", "i2"), tableout=FALSE) {
  if(is.null(criterion)) criterion <- "ALL"

  # calculate cutoff
  if(criterion=="1sd") cutoff <- mean(x[, indexcols], na.rm=T) + sd(x[, indexcols], na.rm=T)
  if(criterion=="2sd") cutoff <- mean(x[, indexcols], na.rm=T) + 2*sd(x[, indexcols], na.rm=T)
  if(criterion=="10perc") cutoff <- as.numeric(quantile(x[, indexcols], 0.9, na.rm=T))
  if(criterion=="5perc") cutoff <- as.numeric(quantile(x[, indexcols], 0.95, na.rm=T))

  # get a table with only the ids of the strongest bonds if a group-level criterion was specified
  if(criterion %in% c("1sd", "2sd", "10perc", "5perc") & tableout ) {
    tres <- na.omit(x[x[,indexcols]>=cutoff,])
    tres <- tres[rev(order(tres[, indexcols])), c(IDcols, indexcols)]
    rownames(tres) <- NULL
    #     idres <- as.matrix(idres[rowSums(!is.na(idres[, 2:ncol(idres)]))>0, ])
    #     sires <- as.matrix(sires[rowSums(!is.na(sires[, 2:ncol(sires)]))>0, ])
    #     r1 <- unlist(apply(as.matrix(idres), 1, function(x) paste(x[1],x[2:length(x)][!is.na(x[2:length(x)])], sep="_@_")))
    #     r2 <- unlist(apply(as.matrix(sires[, 2:ncol(sires)]), 1, function(x)x[!is.na(x)]))
    #     tres <- cbind(r1, r2)
    #     dup <- !duplicated(unlist(lapply(strsplit(tres[,1], "_@_"), function(x) paste(sort(x), collapse="_@_"))))
    #     tres <- tres[dup, ]
    #     tres[rev(order(tres[,2])), ]
    #     tres <- data.frame(i1= unlist(lapply(strsplit(tres[,1], "_@_"), function(x)x[1])), i2= unlist(lapply(strsplit(tres[,1], "_@_"), function(x)x[2])),tres[,2]    )
    #     colnames(tres)[3] <- indexcols
    return(tres)
  }





  # check whether dyads are present twice...
  # ie. whether or not 'directed' friendship
  if(max(table(apply(apply(x[, IDcols], 2, as.character), 1, function(X)paste(sort(X), collapse="_"))))==1) {
    undir <- TRUE
  } else {
    undir <- FALSE
  }


  # all ids
  ids <- sort(unique(c(as.matrix(x[, IDcols]))))
  # create output
  idres <- matrix(ncol=length(ids)+2, nrow=length(ids), ""); idres[,1] <- ids
  sires <- as.data.frame(matrix(ncol=length(ids)+2, nrow=length(ids), NA)); sires[,1] <- ids
  colnames(sires)[3:(length(ids)+2)] <- ids


  if(undir) {
    i=ids[3]
    for(i in ids)  {
      p1 <- which(as.matrix(x[, IDcols[1]])==i)
      p2 <- which(as.matrix(x[, IDcols[2]])==i)
      si <- c(x[p1, indexcols], x[p2, indexcols])
      cn <- c(as.character(x[p1, IDcols[2]]), as.character(x[p2, IDcols[1]]))
      sires[sires[,1]==i , cn] <- si
      rm(p1,p2,si,cn)
    }
  }

  if(!undir) {
    i=ids[1]
    for(i in ids)  {
      p1 <- which(as.matrix(x[, IDcols[1]])==i)
      si <- c(x[p1, indexcols])
      cn <- c(as.character(x[p1, IDcols[2]]))
      sires[sires[,1]==i , cn] <- si
      rm(p1,si,cn)
    }

  }

  idres <- t(apply(sires[, 3:ncol(sires)], 1, function(x)names(sort(x, na.last = T, decreasing = T))))
  sires <- t(apply(sires[, 3:ncol(sires)], 1, function(x)(sort(x, na.last = T, decreasing = T))))

  sires <- sires[,1:(length(ids)-1), drop=FALSE]
  idres <- idres[,1:(length(ids)-1), drop=FALSE]


  if(criterion %in% c("1sd", "2sd", "10perc", "5perc")) {
    ind <- which(!sires>=cutoff)
    sires[ind] <- NA
    idres[ind] <- NA
    coli <- colSums(apply(sires,2,is.na))!=length(ids)
    sires <- as.matrix(sires[, coli])
    idres <- as.matrix(idres[, coli])

  }

  if(is.numeric(criterion)) {
    if(criterion > (length(ids)-1)) criterion <- length(ids)-1
    sires <- sires[, 1:criterion, drop=FALSE]
    idres <- idres[, 1:criterion, drop=FALSE]
  }

  idres[is.na(sires)] <- NA

  nfriends <- apply(sires, 1, function(x)sum(!is.na(x)))
  names(nfriends) <- ids

  ambigorders <- rowSums(t(apply(sires,1,duplicated,incomparables=NA))) > 0

  if(sum(ambigorders) > 0) message("NOTE: partner order for some individuals may not be unique:\n", paste(ids[ambigorders], collapse=", "))



  sires <- data.frame(ID=ids, sires)
  idres <- data.frame(ID=ids, idres)
  if(ncol(sires)==2) colnames(sires)[2] <- "X1"
  if(ncol(idres)==2) colnames(idres)[2] <- "X1"


  return(list(sires, idres, nfriends))

}



