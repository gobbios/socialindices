#' data checks
#'
#' check interaction data for inconsistencies
#'
#' @param b.source data frame with behavioural data
#'
#' @return NULL
#' @export
#'
#' @examples
#' # nothing yet

checkbehaviour <- function(b.source) {
  x <- b.source
  temp <- varname("IDs", x)
  if(length(temp)==2) {
    res <- which(as.character(x[,temp[1]])==as.character(x[,temp[2]]))
    if(length(res)>0) {
      warning("focal and partner were identical")
      res <- x[res, ]
    }
  }

  if(length(temp)==3) {
    res1 <- which(as.numeric(apply(x, 1, function(x)length(unique(c(x[temp[1]], x[temp[2]], x[temp[3]]))))) == 3)
    res2 <- which(as.character(x[,temp[3]])==as.character(x[,temp[2]]))

    if(length(c(res1,res2))>0) {
      warning("focal was not actor or receiver; or actor and receiver were identical")
      res <- x[sort(c(res1,res2)), ]
    }

  }

  if(exists("res")) {
    return(res)
  } else {
    return(paste(as.character(temp), "found in the data set"))
  }
}
