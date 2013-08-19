\name{NiLeDAM}
\alias{NiLeDAM}
\docType{package}
\title{
Monazite Datation for the NiLeDAM team
}
\description{%
This package provides facilities to date monazite using electron microprobe
analysis (EPMA). More precisely, it has three main features:\describe{ 
  \item{ages calculation}{from a set of analyses of the triplets (U,Th,Pb)
contents and corresponding errors, it calculates the age of each analytical
point by solving Equation (1) of Montel \emph{et al.} (1996);} 
	\item{common age(s) estimation}{from these results, it estimates the common
age(s) under the assumption that the analyses come from a given number of age
populations and test for this assumption (\eqn{\chi^2}{chi2}-test);} 
  \item{number of populations estimation}{from the calculated ages, it estimates
the most probable number of age populations from which the analyses come.}} 
}
\details{
\tabular{ll}{
Package: \tab NiLeDAM\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-08-19\cr
License: \tab GPL (>= 2)\cr
Repository: \tab R-Forge\cr
}
The package contains two main functions \code{\link{calculateAges}}, that 
calculate the ages and \code{\link{tests}}, that test for the number of age
populations.

An example is provided with the data \code{\link{srilanka}}, which can be run
using \command{demo(srilanka)}.
}
\author{
  Jean-Marc Montel \email{jean-marc.montel@ensg.inpl-nancy.fr}

  Nathalie Villa-Vialaneix \email{nathalie@nathalievilla.org}
  
  Maintainer: Nathalie Villa-Vialaneix
}
\references{
Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53. }
\examples{
\dontrun{demo(srilanka)}
}
\keyword{htest}
\keyword{aplot}
\keyword{univar}
\keyword{datasets}
