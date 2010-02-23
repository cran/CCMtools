\name{WGP}
\title{Weighted Global Probability (WGP) cost function for precipitation occurrence}
\usage{
WGP(cl.check, cl.proj, DataS.check)
}
\alias{WGP}
\description{
This function calculates the Weighted Global Probability (WGP) cost function for precipitation occurrence. This error is supposed to be due to a misclassification (i.e., the elements should be classified according to sequence cl.check, while they are in practice allocated to the clusters according to the sequence cl.proj).
}
\arguments{

  \item{cl.check}{Reference sequence (i.e., numerical vector) of clusters.}
  \item{cl.proj}{Sequence of clusters in practice.}
  \item{DataS.check}{Precipitation data time series}
}

\value{Returns a numerical vector corresponding to the WGP cost function}

\details{For details about this cost function, see the reference below. 

M. Vrac, P. Yiou. "Weather regimes designed for local precipitation modelling: Application to the Mediterranean bassin". Submitted, JGR-A, 2009
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{DI}}, \code{\link{CWGLI}}}
\examples{
## Example
}
\keyword{cluster}
\keyword{models}
