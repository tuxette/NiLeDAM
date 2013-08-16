\name{plot-methods}
\Rdversion{1.1}
\docType{methods}
\alias{plot-methods}
\alias{plot,oneAgeTest-method}
\alias{plot.oneAgeTest}
\alias{plot,ageTests-method}
\alias{plot.ageTests}
\title{Method \code{"plot"} for Classes \code{"\linkS4class{oneAgeTest}"} and
\code{"\linkS4class{ageTests}"}}
\description{
Display densities of the ages calculated by the function
\code{\link{calculateAges}}, as well as the common age(s) density(ies) as 
returned by the function \code{\link{tests}}.
}
\section{Usage}{plot(x, ...)}
\section{Methods}{
\describe{
\item{\code{signature(object = "oneAgeTest")}}{an object of type
\code{\linkS4class{oneAgeTest}} as produced by the function
\code{\link{tests}}.}
\item{\code{...}}{further arguments passed to the function \code{\link{plot}}.}
}
\describe{
\item{\code{signature(object = "ageTests")}}{an object of type
\code{\linkS4class{ageTests}} as produced by the function
\code{\link{tests}}.
}
\item{\code{...}}{further arguments passed to the function \code{\link{plot}}.}
}}
\details{The estimated densities are Gaussian, as supposed by the model 
described in Montel \emph{et al.} (1996). Each density is referenced by its 
number to help identify outliers or invalid measurements.

Also note that if an object of class \code{\linkS4class{oneAgeTest}} is passed
to the function, the common age(s) densities is displayed even if the test of
the function \code{\link{tests}} is rejected.

If arguments \option{col}, \option{lwd} and \option{lty} are supplied, they are
used to display the density(ies) of the common age(s).
}
\author{
  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53. }
\seealso{\code{\link{tests}}, \code{\linkS4class{oneAgeTest}},
\code{\linkS4class{ageTests}}}
\examples{
data(srilanka)
calculated.ages <- calculateAges(srilanka, nloops=10)
res.tests <- tests(calculated.ages,1,3)
plot(res.tests, main="Densities", col="blue")
}
\keyword{methods}
