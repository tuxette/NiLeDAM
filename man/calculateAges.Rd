\name{calculateAges}
\alias{calculateAges}
\title{Calculate the ages from electron microprobe measurements}
\usage{
  calculateAges(measures, nloops = 1000, level = 0.05, verbose = TRUE,
    seed = NULL)
}
\arguments{
  \item{measures}{a data.frame object with one electron microprobe measurement
  by row and with columns U, corresponding error for U, Th, corresponding error
  for Th, Pb, corresponding error for Pb, all expressed in ppm. See an example
  with \command{data(srilanka)}.}

  \item{nloops}{the number of Monte Carlo (MC) simulations used to estimate the
  confidence intervals for the ages. Default is \code{1000}.}

  \item{level}{the level of significance of the confidence intervals for the
  ages. Default is \code{0.05}.}

  \item{verbose}{logical; activates the verbose mode. Default is \code{TRUE}.}

  \item{seed}{if supplied, initialize the random seed. Default is \code{NULL}
  (the random seed is not initialized).}
}
\value{
  an object of class \code{\linkS4class{ages}}.
}
\description{
  This function calculates the ages, confidence intervals and standard
  deviations from triplets (U,Th,Ph), obtained by electron microprobe, given
  together with corresponding errors.
}
\details{
  the ages are calculated by solving the Equation (1) of Montel \emph{et al.}
  (1996). The equation is solved by the Broyden method implemented in the
  \code{\link[nleqslv:nleqslv]{nleqslv}} function.

  The standard deviations and the confidence intervals are calculated using a MC
  approach: randomized observations of the triplets are generated from normal
  distributions with standard deviations equal to
  \eqn{\frac{\textrm{error}}{2}}{error/2} where 'error' denotes the error in the
  measurement of Th, U or Pb, passed in \option{measures}. Standard deviations
  are estimated by the empirical standard deviations and confidence intervals by
  quantiles for probabilities \eqn{\frac{\textrm{\texttt{level}}}{2}}{level/2}
  and \eqn{1-\frac{\textrm{\texttt{level}}}{2}}{1-level/2}, respectively.
}
\note{
  You should use at least 1000 MC simulations otherwise the estimated confidence
  intervals and standard deviations will not be reliable. Such simulations can
  take a few seconds/minutes for fifty or so triplets and corresponding errors.
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53. }
\author{
  Jean-Marc Montel \email{jean-marc.montel@ensg.inpl-nancy.fr}

  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
}
\seealso{
  \code{\linkS4class{ages}} \code{\link{tests}}
}
\examples{
## Load the data
data(srilanka)
## Calculate the ages
calculateAges(srilanka, nloops=10)
}
\keyword{htest}
