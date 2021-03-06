% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poker.R
\name{dotTripRanker}
\alias{dotTripRanker}
\title{dotTripRanker}
\usage{
dotTripRanker(oneHand)
}
\arguments{
\item{oneHand}{: \tabular{ll}{a sorted hand with ranks only as
vector[7] \tab \cr \tab
col1: rank of card 1 in \{2, ... , 14\} \cr \tab
col2: rank of card 2 \cr \tab
. \cr \tab
. \cr \tab
. \cr \tab
col7: rank of card 7}}
}
\value{
tripRank : the rank of the pair as
											vector
}
\description{
Determine the rank of the three of a kind.  Note: dotTripRanker requires a hand with a score of 4 (i.e., three of a kind).
}
\examples{
dotTripRanker(c(9,7,5,3,3,3,2))
}
