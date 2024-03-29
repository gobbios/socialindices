% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datagen.R
\name{datagen}
\alias{datagen}
\title{generate random social interaction data}
\usage{
datagen(
  no = 50,
  ni = 5,
  beh = c("appr", "gro", "supp", "prox"),
  behw = c(0.2, 0.3, 0.1, 0.4),
  behdur = c(F, T, F, T),
  biaswithin = 1,
  friendprop = 0.1,
  nadd = 0,
  presence = NULL
)
}
\arguments{
\item{no}{number of interactions to be created}

\item{ni}{number of individuals}

\item{beh}{character, \code{beh} determines behaviours for the "positive" interactions of \code{datagen}}

\item{behw}{numeric, weights of occurences of behaviours in \code{datagen}}

\item{behdur}{logical, whether behaviours have durations}

\item{biaswithin}{bias parameter WTIHIN dyads: if 1: roughly 66\% of directionality (biased towards the ID that comes first in alphabet); if 0: no bias, i.e. 50\% bias}

\item{friendprop}{proportion of dyads with preferential (more frequent/longer) interactions}

\item{nadd}{numeric, additional number of non-focal IDs}

\item{presence}{numeric of length = 2, (1) proportion of IDs affected by absence, (2) proportion of days (relative to date range) for absence}
}
\value{
list with six items: (1) data.frame with interaction data, (2) data.frame with observation time data, (3) data.frame with presence data, (4) character with "friend" dyads, (5) character with focal individuals, (6) non-focal individuals
}
\description{
generate random social interaction data
}
\details{
The process of inventing interactions within these functions starts with creating a data collection protocol. This protocol is modelled on focal animal sampling, in which the \code{ni} individuals serve as focal animals, and it ends up with a list of "protocols" for a given focal ID on several dates associated with pseudo-randomized observation time (i.e. protocol duration). See examples...

The argument \code{friendprop} introduces higher rates or longer durations of interaction in this proportion of dyads as compared to other dyads. So far, this applies only to focal-focal dyads and won't introduce focal-nonfocal "friends" even if \code{nadd} is not zero.
}
\examples{
x <- datagen(no=20, ni=3)
# observation time table
x$ot
# interactions
x$dataseq
}
