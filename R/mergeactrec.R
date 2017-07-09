#' merge actor and recipient
#'
#' merge actor and recipient (independent of focal)
#'
#' @param x a data set with behavioural data, including actor and recipient or giver/receiver columns (in addition to focal column)
#'
#' @return same as \code{x} but with the additional column \code{$XPARTNER}. If there was an actor/receiver column pair in the original data, this new column will be the non-focal of the two. Additionally, a column \code{$focwasact} is added, which indicates whether the focal was the actor (\code{TRUE}) or the recipient ({\code{FALSE}}).
#' For consistency and easier handling in the CSI function, also a \code{$XFOCAL} column is generated which is identical to the original focal column.
#' If there is only a partner column in the data set, the same new columns will be generated (again for easier handling, i.e. consistency in the CSI function). The \code{$focwasact} will be all \code{TRUE} in that case...
#' @details
#' The point of this function is to generate a data set that can be handled in the same way, regardless of whether the data are in focal/partner or focal/actor/receiver format.
#'
#' @examples
#' data(dataset3)
#' head(.mergeactrec(dataset3$dataseq))

.mergeactrec <- function(x) {
  # how many ID columns are there and what are their names
  cn <- varname("IDs", x)

  # first case, three: focal, actor, receiver
  if(length(cn) == 3) {
    # run an additional test
    # check whether the focal was either actor or receiver (has to be one of the two)
    XX <- apply(x[, cn], 2, as.character)
    TEST <- unlist(lapply(apply(XX, 1, function(x)x[x[cn[1]] != x[cn]]), length))
    if(max(TEST)>1) {
      xlines <- paste(which(TEST>1), collapse = ", ")
      stop(paste("in the following lines, the focal was neither actor nor receiver:", "\n", xlines))
    }

    x$XPARTNER <- as.character(apply(x[, cn], 1, function(x)x[x[cn[1]] != x[cn]]))
    x$XFOCAL <- as.character(x[, cn["focal"]])
    x$focwasact <- as.character(x[,cn["focal"]])==as.character(x[, cn["actor"]])
  }
  # x$XPARTNER != x$XFOCAL
  # second case, two: focal, partner
  if(length(cn) == 2) {
    x$XFOCAL <- as.character(x[, cn["focal"]])
    x$XPARTNER <- as.character(x[, cn["partner"]])
    x$focwasact <- TRUE
  }


  return(x)
}
