\name{learn.and.project.clusters}
\title{Learning of attribution of clusters, and projection of new days to clusters}
\usage{
learn.and.project.clusters(DataCalibration, DataToBeProjected, cl.calibration, allocmet, DataS.Calibration=NaN)
}
\alias{learn.and.project.clusters}
\description{
This function (1) learns how to attribute days to clusters based on the sequence of predictors and associated sequence of clusters.
}
\arguments{

  \item{DataCalibration}{Values of the predictor variable for the calibration set (can be a matrix).}
  \item{DataToBeProjected}{Values of the predictor variable for the projection set.}
  \item{cl.calibration}{Numerical vector corresponding to the sequence of clusters (i.e., calibration set).}
  \item{allocmet}{Name of the attribution method.(The 12 possibilities are: "Euclid.dist.A", "Euclid.dist.w1", "Euclid.dist.w2", "CART.A", "CART.w", "CART.A.and.w", "knnA", "knnA10", "Gaussian.A", "Gaussian.w", "MM", "MMw")}
  \item{DataS.Calibration}{Values of other predictor variables for the calibration set. This is sometimes needed, according to the attribution method (allocmet) to be used (needed for "Euclid.dist.w1", "Euclid.dist.w2", "CART.w", "CART.A.and.w", "Gaussian.w", "MMw").}
}


\value{Returns a list with two objects:
  \item{cl}{The sequence of clusters defined from the predictors for the projection set.}
  \item{tot}{Number of elements per cluster for projection set.}
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\examples{
## Example
}
\keyword{cluster}
