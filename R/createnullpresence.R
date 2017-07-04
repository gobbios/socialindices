#' create null data
#'
#' @param IDs character, IDs
#' @param from start date
#' @param to end date
#' @param propID proportion of IDs affected by absence
#' @param propabs proportion of days (relative to date range) for absence
#'
#' @return data.frame, i.e. "presence matrix"
#' @export
#'
#' @examples
#' createnullpresence(letters[1:5], "2000-01-01", "2000-01-05")
#' createnullpresence(letters[1:5], "2000-01-01", "2000-01-05", 0.5, 0.5)

createnullpresence <- function(IDs, from, to, propID=NULL, propabs=NULL) {
  res <- data.frame(date=seq(as.Date(from), as.Date(to), by="day"))
  for(i in IDs) res[,i] <- 1

  if(!is.null(propID) & !is.null(propabs)) {
    # how many and which IDs affected
    ifelse(round(length(IDs)*propID[1])!=0, pID <- sample(IDs, round(length(IDs)*propID[1])), pID <- sample(IDs, 1))

    # modify presence matrix
    # duration of absence:
    dur <- round(propabs*nrow(res)); if(dur < 2) dur <- 2

    i=pID[1]
    for(i in pID) {
      # random reference date (line)
      rdate <- sample(1:nrow(res), 1)

      # assign whether "rdate" is exit or intro
      type <- sample(c("exit","intro"), 1)

      # type intro
      if(type=="intro") {
        # exit date
        exdate <- rdate + dur; if(exdate > nrow(res)) exdate <- nrow(res)
        res[rdate:exdate, i] <- 0
      }

      # type exit
      if(type=="exit") {
        # intro date
        indate <- rdate - dur; if(indate < 1) indate <- 1
        res[indate:rdate, i] <- 0
      }
    }
  }
  if(0 %in% colSums(res[,IDs])) warning("individuals included that were never present...", call.=FALSE)
  if(0 %in% rowSums(res[,IDs])) warning("days included without anyone present...", call.=FALSE)
  return(res)
}
