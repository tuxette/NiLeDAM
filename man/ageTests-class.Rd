\name{ageTests-class}
\Rdversion{1.1}
\docType{class}
\alias{ageTests-class}
\alias{ageTests}
\alias{print,ageTests-method}
\alias{print.ageTests}
\alias{show,ageTests-method}
\alias{show.ageTests}
\alias{summary,ageTests-method}
\alias{summary.ageTests}

\title{Class \code{"ageTests"}}
\description{
Results of \eqn{\chi^2}{chi2}-tests testing to which number of populations the
ages calculated by the function \code{\link{calculateAges}} are the most lickely
to come from. The test is the one described in the article Montel \emph{et al.}
(1996).
}
\section{Objects from the Class}{
Object from this class are created by the function \code{\link{tests}} applied
to an object of class \code{\link{ages}} when the option \option{nbmax} is not
set to \code{NULL} but to an integer larger than \option{nbmin}.
}
\section{Slots}{
  \describe{
    \item{\code{nb.pop}:}{Object of class \code{"vector"} that contains the 
tested numbers of populations.}
    \item{\code{best.nb}:}{Object of class \code{"numeric"} that contains the
most probable number of populations, which is the smallest number for which the
\eqn{\chi^2}{chi2}-test is accepted.}
    \item{\code{best.res}:}{Object of class \code{"\linkS4class{oneAgeTest}"}
that contains the result of the test for \code{best.nb} populations.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "ageTests")}: displays a summary of the
results of the test.}
    \item{show}{\code{signature(object = "ageTests")}: displays a summary of the
results of the test.}
    \item{summary}{\code{signature(object = "ageTests")}: displays a summary of
the results of the test.}
	 }
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.}
\author{
  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\seealso{
\code{\linkS4class{ages}}, \code{\link{tests}}, \code{\link{plot.ageTests}}
}
\examples{
showClass("ageTests")
## Example
data(srilanka)
calculated.ages <- calculateAges(srilanka, nloops=10)
tests(calculated.ages,nbmax=3)
}
\keyword{classes}
