\name{tests-methods}
\Rdversion{1.1}
\docType{methods}
\alias{tests-methods}
\alias{tests,ages-method}
\alias{tests}
\title{Method \code{"tests"} for Class \code{"\linkS4class{ages}"}}
\description{
Test if a set of ages belongs to a given number of populations or
alternatively find out the most probable number of populations within a range.
}
\section{Usage}{tests(object, nbmin=1, nbmax=NULL, level=object@level,
                      verbose=TRUE)}
\section{Methods}{
\describe{
\item{\code{signature(object = "ages")}}{an object of type
\code{\linkS4class{ages}} as produced by the function
\code{\link{calculateAges}}.
}
\item{nbmin}{numeric. Minimum tested number of populations.}
\item{nbmax}{numeric. Maximum tested number of populations.}
\item{level}{numeric. Confidence interval level. Default is equal to 
\code{object@level}, i.e., to the level of confidence used to calculate the
ages.}
\item{verbose}{logical. Activates the verbose mode.}
}}
\details{The function successively tests all values from \code{nbmin} to
\code{nbmax} and keeps the smallest accepted one (returns an error if all
supplied values are rejected). In this case, the output value is an object of
class \code{\linkS4class{ageTests}}.

If \code{nbmax} is \code{NULL}, the results are kept even if the test is 
rejected. In this case, the output value is an object of class 
\code{\linkS4class{oneAgeTest}}.

The performed tests are those described in Montel \emph{et al.} (1996).}
\value{An object of class \code{\linkS4class{ageTests}} or
\code{\linkS4class{oneAgeTest}} depending on the value of \code{nbmax} (see
section "Details").}
\author{
  Jean-Marc Montel \email{jean-marc.montel@ensg.inpl-nancy.fr}

  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53. }
\seealso{\code{\link{calculateAges}}, \code{\linkS4class{oneAgeTest}},
\code{\linkS4class{ageTests}}}
\examples{
data(srilanka)
calculated.ages <- calculateAges(srilanka, nloops=10)
tests(calculated.ages)
tests(calculated.ages,1,3)
}
\keyword{methods}
