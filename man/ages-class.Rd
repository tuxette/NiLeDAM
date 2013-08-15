\name{ages-class}
\Rdversion{1.1}
\docType{class}
\alias{ages-class}
\alias{ages}
\alias{print,ages-method}
\alias{print.ages}
\alias{show,ages-method}
\alias{show.ages}
\alias{summary,ages-method}
\alias{summary.ages}

\title{Class \code{"ages"}}
\description{
A class for the results of the \code{\link{calculateAges}} function. It contains
the ages calculated for each analytical point from triplets (U,Th,Pb) and
corresponding errors.
}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{calculateAges}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} that contains the
original triplets (U,Th,Pb) and corresponding errors.}
    \item{\code{ages}:}{Object of class \code{"vector"} that contains the 
ages calculated for each analytical point.}
    \item{\code{ci}:}{Object of class \code{"matrix"}, which is a two-column
matrix that contains the bounds of the confidence intervals for each age of the
slot \code{ages}. These are estimated from a MC simulation method as described
in \code{\link{calculateAges}}.}
    \item{\code{sd}:}{Object of class \code{"vector"} that contains the standard
deviations for each age of the slot \code{ages}. These are estimated from a MC 
simulation method as described in \code{\link{calculateAges}}.}
    \item{\code{nloops}:}{Object of class \code{"numeric"} that stores the 
number of MC simulations used to estimate the confidence intervals and the 
standard deviations.}
    \item{\code{level}:}{Object of class \code{"numeric"} that stores the level
of the confidence intervals.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "ages")}: display a summary of the results
of the ages calculation.}
    \item{show}{\code{signature(object = "ages")}: display a summary of the
results of the ages calculation (same as \code{print.ages}).}
    \item{summary}{\code{signature(object = "ages")}: display a summary of the
results of the ages calculation (same as \code{print.ages}).}
	 }
}
\author{
Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\seealso{
\code{\link{calculateAges}}, \code{\link{tests}}
}
\examples{
showClass("ages")
## Example on the srilanka dataset
data(srilanka)
calculated.ages <- calculateAges(srilanka, nloops=10)
calculated.ages
}
\keyword{classes}
