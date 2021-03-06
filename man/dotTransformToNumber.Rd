% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poker.R
\name{dotTransformToNumber}
\alias{dotTransformToNumber}
\title{dotTransformToNumber}
\usage{
dotTransformToNumber(rank, suit)
}
\arguments{
\item{rank}{: \tabular{ll}{rank of card y as
integer in \{2, ... , 14\} \tab \cr \tab
 2 = deuce \cr \tab
 . \cr \tab
 . \cr \tab
 . \cr \tab
 11 = jack \cr \tab
 12 = queen \cr \tab
 13 = king \cr \tab
 14 = ace}}

\item{suit}{: \tabular{ll}{suit of card y as
integer in \{1, 2, 3, 4\} \tab \cr \tab
 1 = spade \cr \tab
 2 = club \cr \tab
 3 = heart \cr \tab
 4 = diamond}}
}
\value{
y: number corresponding to card as
											integer in \{1, 2, ... , 52\}
}
\description{
Determine the card from a rank and suit.
}
\examples{
dotTransformToNumber(2,1)
dotTransformToNumber(14,1)
dotTransformToNumber(2,2)
dotTransformToNumber(14,2)
}
