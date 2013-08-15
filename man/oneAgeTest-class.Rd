\name{oneAgeTest-class}
\Rdversion{1.1}
\docType{class}
\alias{oneAgeTest-class}
\alias{oneAgeTest}
\alias{print,oneAgeTest-method}
\alias{print.oneAgeTest}
\alias{show,oneAgeTest-method}
\alias{show.oneAgeTest}
\alias{summary,oneAgeTest-method}
\alias{summary.oneAgeTest}

\title{Class \code{"oneAgeTest"}}
\description{
Results of a \eqn{\chi^2}{chi2}-test testing if the ages calculated by the 
function \code{\link{calculateAges}} come from a given number of populations.
The test is the one describes in the article Montel \emph{et al.} (1996).
}
\section{Objects from the Class}{
Object from this class are created by the function \code{\link{tests}} applied
to an object of class \code{\link{ages}} when the option \option{nbmax} is set
to \code{NULL}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"\link{ages}"} as created by the
function \code{\link{calculateAges}}. This is the object passed as an argument
to the function \code{\link{tests}}.}
    \item{\code{ages}:}{Object of class \code{"vector"} that contains the 
estimated common age(s) of the population(s). Its size is equal to the argument
\option{nbmin} passed to the function \code{\link{tests}}.}
    \item{\code{sd}:}{Object of class \code{"vector"} that contains the 
estimated standard deviations of the common age(s) of the population(s). Its
size is equal to the argument \option{nbmin} passed to the function
\code{\link{tests}}.}
    \item{\code{ic}:}{Object of class \code{"matrix"} having \option{nbmin} rows
and two columns corresponding to the confidence intervals at level
\option{level} as passed as an argument to the function \code{\link{tests}}.}
    \item{\code{S}:}{Object of class \code{"numeric"} \eqn{\chi^2}{chi2} test
statistic of the test.}
    \item{\code{thres}:}{Object of class \code{"numeric"} probability threshold
of the test.}
    \item{\code{df}:}{Object of class \code{"numeric"} degree of freedom of the
test.}
    \item{\code{level}:}{Object of class \code{"numeric"} confidence level of
the test. Passed as an argument to the function \code{\link{tests}}.}
    \item{\code{h0}:}{Object of class \code{"logical"} result of the test: if 
\code{h0} is \code{TRUE}, the test is accepted at level \option{level} and if
\code{h0} is \code{FALSE}, the test is rejected at level \option{level}.}
    \item{\code{which.pop}:}{Object of class \code{"vector"} the contains the
population number for each analytical point. \code{1} corresponds to the 
population with the first age in \code{ages}, \code{2} to the population with
the second age in \code{ages}...}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "oneAgeTest")}: displays a summary of the
results of the test.}
    \item{show}{\code{signature(object = "oneAgeTest")}: displays a summary of
the results of the test.}
    \item{summary}{\code{signature(object = "oneAgeTest")}: displays a summary
of the results of the test.}
	 }
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53. }
\author{
  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\seealso{
\code{\linkS4class{ages}}, \code{\link{tests}}, \code{\link{plot.oneAgeTest}}
}
\examples{
showClass("oneAgeTest")
## Example
data(srilanka)
calculated.ages <- calculateAges(srilanka, nloops=10)
tests(calculated.ages)
}
\keyword{classes}
