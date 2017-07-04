#' create null data
#'
#' @param IDs character, IDs
#' @param from start date
#' @param to end date
#'
#' @return data.frame, i.e. observation time table
#' @export
#'
#' @examples
#' createnullot(letters[1:5], "2000-01-01", "2000-01-05")

createnullot <- function(IDs, from, to) {
  dts <- seq(as.Date(from), as.Date(to), by="day")
  res <- expand.grid(IDs, dts)
  colnames(res) <- c("focal", "date")
  res$ot <- 1
  return(res)
}
