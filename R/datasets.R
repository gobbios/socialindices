#' example data sets for socialindices
#'
#' Fictious examples of data sets to calculate social bonds
#'
#' @name datasets
#' @aliases dataset1 dataset2 dataset3 dataset4 domseq4
#' @docType data
#' @format
#' \itemize{
#' \item \code{dataseq} data.frame with interaction data
#' \item \code{ot} data.frame with observation time of individual protocols
#' \item \code{presence} presence table of focal individuals
#' \item \code{presence2} presence table of nonfocal individuals
#' \item \code{prefdyads} dyads with higher rate/longer duration of interactions
#' \item \code{focals} character, focal individuals
#' \item \code{nonfocs} character, nonfocal individuals
#' }
#' @keywords datasets
#' @details The data sets included have been generated with the \code{\link{datagen}} function. See examples for the code...
#' @examples
#' \dontrun{
#' dataset1 <- datagen(no=2000, ni=10, nadd=20, friendprop=0.2, presence=c(0.5, 0.4))
#' dataset2 <- datagen(no=2000, ni=30, nadd=0,  friendprop=0.2, presence=c(0.5, 0.4))
#' dataset3 <- datagen(no=200,  ni=3,  nadd=3,  friendprop=0.2, presence=c(0.5, 0.4))
#' dataset4 <- datagen(1000, ni=14)
#' domseq4 <- domgen(dataset4, 500, drawprop=0)
#' }
"dataset1"
"dataset2"
"dataset3"
"dataset4"
"domseq4"
