% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createnullpresence.R
\name{createnullpresence}
\alias{createnullpresence}
\title{create null data}
\usage{
createnullpresence(IDs, from, to, propID = NULL, propabs = NULL)
}
\arguments{
\item{IDs}{character, IDs}

\item{from}{start date}

\item{to}{end date}

\item{propID}{proportion of IDs affected by absence}

\item{propabs}{proportion of days (relative to date range) for absence}
}
\value{
data.frame, i.e. "presence matrix"
}
\description{
create null data
}
\examples{
createnullpresence(letters[1:5], "2000-01-01", "2000-01-05")
createnullpresence(letters[1:5], "2000-01-01", "2000-01-05", 0.5, 0.5)
}
