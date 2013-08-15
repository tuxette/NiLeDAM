\name{srilanka}
\alias{srilanka}
\docType{data}
\title{
An example data set: electron microprobe data.
}
\description{
This dataset is kindly provided by Anne-Magali Seydoux-Guillaume
\email{anne-magali.seydoux@get.obs-mip.fr} and has been published in 
Seydoux-Guillaume \emph{et al.} (2012).
}
\usage{data(srilanka)}
\format{
  A data frame with 32 observations on the following 6 variables:
  \describe{
    \item{\code{U}}{U concentration (ppm)}
    \item{\code{errU}}{error on the measurement of U concentration}
    \item{\code{Th}}{Th concentration (ppm)}
    \item{\code{ErrTh}}{error on the measurement of Th concentration}
    \item{\code{Pb}}{Pb concentration (ppm)}
    \item{\code{ErrPb}}{error on the measurement of Pb concentration}
  }
}
\details{
The first 8 observations are group control data. When testing if all the 
observations are issued from a single population, the assumption is thus 
rejected. Removing the first 8 observations leads to obtain a positive answer 
when testing if the observations come from the same population. See 
\code{demo(srilanka)}.
}
\references{
Seydoux-Guillaume A.M., Montel J.M., Bingen B., Bosse V., de Parseval P.,
Paquette J.L., Janots E., Wirth R. (2012) Low-temperature alteration of
monazite: fluid mediated coupled dissolution-precipitation, irradiation damage
and disturbance of the U-Pb and Th-Pb chronometers. \emph{Chemical Geology}, 
\bold{330--331}, 140--158.
}
\examples{
data(srilanka)
# with control group data
summary(srilanka)
# without control group data
summary(srilanka[9:32,])
}
\keyword{datasets}
