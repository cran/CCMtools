\name{DI}
\title{Daily Intensity (DI) cost function for precipitation}
\usage{
DI(cl.calibration, cl.check, cl.proj, DataS.Calibration, DataS.check)
}
\alias{DI}
\description{
This function calculates the Daily Intensity (DI) cost function for precipitation. This error is supposed to be due to a misclassification (i.e., the elements should be classified according to sequence cl.check, while they are in practice allocated to the clusters according to the sequence cl.proj).
}
\arguments{

  \item{cl.calibration}{Sequence of clusters for time period 1 (TP1)}
  \item{cl.check}{Reference sequence (i.e., numerical vector) of clusters (i.e., this is the sequence we should have) for time period 2 (TP2).}
  \item{cl.proj}{Sequence of clusters in practice for TP2.}
  \item{DataS.Calibration}{Precipitation data time series for TP1.}
  \item{DataS.check}{Precipitation data time series for TP2.}
}

\value{Returns a list with arguments:
  \item{DI}{Daily Intensity (DI) error costs, depending on station s and day d.}
  \item{MDI}{Means DI costs, depending on station s.}
  \item{RMDI}{Regional MDI providing a global (spatial and temporal) view of the DI error cost.}

}

\details{For details about these cost functions, see the reference below. 

M. Vrac, P. Yiou. "Weather regimes designed for local precipitation modelling: Application to the Mediterranean bassin". Submitted, JGR-A, 2009
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{DI}}, \code{\link{WGP}}}
\examples{
## Example
}
\keyword{cluster}
\keyword{models}
