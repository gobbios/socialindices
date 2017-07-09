#' limit dataset to date range
#'
#' @param x data.frame with a date column in it \code{YYYY-MM-DD}
#' @param daterange character of length 2, e.g. \code{c("2010-01-01", "2010-04-04")}
#' @param omitdatecol should the actual date column be omitted from the output, by default no (\code{FALSE})
#'
#' @return a subset of \code{x}
#' @export
#'
#' @examples
#' data(dataset3)
#' limit2daterange(dataset3$dataseq, c("2000-01-01", "2000-01-14"))
#' limit2daterange(dataset3$presence, c("2000-01-01", "2000-01-14"))
#' limit2daterange(dataset3$presence, c("2000-01-01", "2000-01-14"), TRUE)

limit2daterange <- function(x, daterange, omitdatecol=FALSE) {
  if(is.null(daterange)) return(x)
  dcol <- varname("date", x)
  x <- x[as.Date(as.character(x[, dcol])) >= as.Date(daterange[1]) & as.Date(as.character(x[, dcol])) <= as.Date(daterange[2]), ]
  if(is.data.frame(x)) x <- droplevels(x)

  if(omitdatecol) x <- x[, -c(!colnames(x) %in% dcol)]
  rownames(x) <- NULL
  return(x)
}
