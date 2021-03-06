% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poker.R
\name{dotFourOfAKindRanker}
\alias{dotFourOfAKindRanker}
\title{dotFourOfAKindRanker}
\usage{
dotFourOfAKindRanker(oneHand)
}
\arguments{
\item{oneHand}{: \tabular{ll}{the ranks of one 7 card hand as
vector[7] \tab \cr \tab
col1: rank of card 1 in \{2, ... , 14\} \cr \tab
col2: rank of card 2 \cr \tab
. \cr \tab
. \cr \tab
. \cr \tab
col7: rank of card 7}}
}
\value{
\tabular{ll}{fourOfAKindRank : the ranks of the quads and the high kicker as
											vector \tab \cr \tab
											col1: the rank of the quads \cr \tab
											col2: the rank of the kicker}
}
\description{
Determine the rank of the four of a kind and the kicker. This functions assumes ranks are sorted in decreasing order.
}
\examples{
dotFourOfAKindRanker(c(14,14,14,14,10,7,6))
dotFourOfAKindRanker(sort(c(10,14,6,14,7,14,14),decreasing=TRUE))
}
