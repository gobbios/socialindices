#' coresidency of a dyad
#'
#' @param ID1 character, IDs
#' @param ID2 character, IDs
#' @param daterange character of length 2, e.g. \code{c("2010-01-01", "2010-04-04")}
#' @param presence character: name of presence data frame (i.e. in quotes), or matrix without date column. in the latter case make sure that \code{pr.dates=} is supplied
#' @param outp \code{"abs"} (returning number of days), or \code{"prop"} (proportion of days given the date range), or \code{"2days"} (both IDs present on both dates), see details
#' @param pr.dates optional character vector which corresponds to the \code{presence=} matrix. Should speed up computations in randomization tests if presence of two individuals on two specific days is needed.
#'
#' @details
#' the \code{outp="2days"} option is useful only in the context of randomization procedures, and as such probably not useful for normal applications of the function.
#'
#' if the \code{pr.dates=} argument is used this implies that the actual presence matrix is a true numeric matrix while the corresponding dates are in a separate vector (of the same length as the number of rows of the presence data set). So, it's probably also an argument that is not useful for the typical use.
#'
#' @return numeric, or logical
#' @export
#'
#' @examples
#' # load an example data set:
#' data(dataset3)
#' # extract the presence table from it:
#' pres <- dataset3$presence
#' head(pres)
#' cores("a", "d", daterange = c("2000-01-01", "2000-01-06"), presence = "pres", outp = "abs")
#' cores("a", "d", daterange = c("2000-05-01", "2000-06-01"), presence = "pres", outp = "prop")


cores <- function(ID1, ID2, daterange, presence, outp, pr.dates=NULL) {
  if(outp %in% c("abs", "prop")) {
    pres <- get(presence)
    colnames(pres) <- toupper(colnames(pres))
    ID1 <- toupper(ID1); ID2 <- toupper(ID2)
    if(sum(c(ID1 %in% colnames(pres), ID2 %in% colnames(pres)))!=2) stop ("not both IDs found in presence data...")
    if(length(daterange)==1) pres <- pres[pres$DATE==daterange, ]
    if(length(daterange)==2) pres <- pres[which(pres$DATE %in% seq(from=as.Date(daterange[1]), to=as.Date(daterange[2]), by="day"))     , ]

    temp <- pres[, c(ID1, ID2)]


    if(outp=="abs") return(sum(rowSums(temp)==2))
    if(outp=="prop") return(sum(rowSums(temp)==2)/nrow(temp))
  }


  if(outp == "2days") {
    #vn <- varname("date", presencedata)
    #return(sum(presencedata[presencedata[, vn]==daterange[1] , c(ID1, ID2)] + presencedata[presencedata[, vn]==daterange[2] , c(ID1, ID2)])==4)
    i <- which(pr.dates %in% daterange)
    return(sum(presence[i, c(ID1, ID2)])==4)
  }


}

