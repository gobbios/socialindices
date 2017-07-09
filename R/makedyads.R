#' dyads from single individual IDs
#'
#' @param IDs character, IDs
#'
#' @return a matrix with four columns, first two are the single IDs, third is the dyad in alphabetical order, fourth is dyad in reverse
#' @export
#' @examples
#' makedyads(LETTERS[4:1])
#'
#'

makedyads <- function(IDs) {
  res <- t(combn(IDs, 2))
  res <- cbind(res, apply(res, 1, function(x)paste(sort(c(x[1], x[2])), collapse="_@_")),
               apply(res, 1, function(x)paste(rev(sort(c(x[1], x[2]))), collapse="_@_")))
  return(res)
}
