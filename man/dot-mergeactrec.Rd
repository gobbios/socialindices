% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeactrec.R
\name{.mergeactrec}
\alias{.mergeactrec}
\title{merge actor and recipient}
\usage{
.mergeactrec(x)
}
\arguments{
\item{x}{a data set with behavioural data, including actor and recipient or giver/receiver columns (in addition to focal column)}
}
\value{
same as \code{x} but with the additional column \code{$XPARTNER}. If there was an actor/receiver column pair in the original data, this new column will be the non-focal of the two. Additionally, a column \code{$focwasact} is added, which indicates whether the focal was the actor (\code{TRUE}) or the recipient ({\code{FALSE}}).
For consistency and easier handling in the CSI function, also a \code{$XFOCAL} column is generated which is identical to the original focal column.
If there is only a partner column in the data set, the same new columns will be generated (again for easier handling, i.e. consistency in the CSI function). The \code{$focwasact} will be all \code{TRUE} in that case...
}
\description{
merge actor and recipient (independent of focal)
}
\details{
The point of this function is to generate a data set that can be handled in the same way, regardless of whether the data are in focal/partner or focal/actor/receiver format.
}
\examples{
data(dataset3)
head(socialindices2:::.mergeactrec(dataset3$dataseq))
}
