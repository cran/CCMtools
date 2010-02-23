\name{CWGLI}
\title{Conditional Weighted Global Log-Intensity (CWGLI) cost function for precipitation}
\usage{
CWGLI(cl.check, cl.proj, DataS.check)
}
\alias{CWGLI}
\description{
This function calculates the Conditional Weighted Global Log-Intensity (CWGLI) cost function for precipitation. This error is supposed to be due to a misclassification (i.e., the elements should be classified according to sequence cl.check, while they are in practice allocated to the clusters according to the sequence cl.proj).
}
\arguments{

  \item{cl.check}{Reference sequence (i.e., numerical vector) of clusters (i.e., this is the sequence we should have).}
  \item{cl.proj}{Sequence of clusters in practice.}
  \item{DataS.check}{Precipitation data time series}
}

\value{Returns a numerical vector corresponding to the CWGLI cost function}

\details{For details about this cost function, see the reference below. 

M. Vrac, P. Yiou. "Weather regimes designed for local precipitation modelling: Application to the Mediterranean bassin". Submitted, JGR-A, 2009
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{DI}}, \code{\link{WGP}}}
\examples{
## Example
}
\keyword{cluster}
\keyword{models}
