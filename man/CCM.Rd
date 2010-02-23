\name{CCM}
\title{Clustering through "Correlation Clustering Model" (CCM) method}
\usage{
CCM(Nc, NS, DataA.tbc,DataS.tbc,NN, DataStation, init="block", ITmax=15, rq=0)
}
\alias{CCM}
\description{
This function performs a simultaneous clustering of two matched datasets (e.g., daily local- and large-scale atmospheric data), such that each cluster maximises the correlation between the two datasets. This clustering is based on a mixture of canonical correlation analyses (CCAs).
}
\arguments{

  \item{Nc}{Number of clusters required.}
  \item{NS}{Number of locations (i.e., weather stations) for the local-scale time series.}
  \item{DataA.tbc}{Dataset corresponding to the large-scale data to be clustered. This is a matrix M*NN, where M corresponds to the number of large-scale locations (e.g., GCM or RCM grid cells), and NN to the length of the time series (e.g., number of days). Note that this matrix is used in CCM without any transformation. For example, if a principal component analysis (PCA) has to be performed, this must be done BEFORE entering CCM.}
  \item{DataS.tbc}{Dataset corresponding to the local-scale (station) data to be clustered. This is a matrix NS*NN. Similarly as for DataA.tbc, note that this matrix is used in CCM without any transformation, and that if a PCA has to be performed, this must be done BEFORE entering CCM.}
  \item{NN}{Length of the time series (e.g, number od days for daily time series).}
  \item{DataStation}{Local-scale (stations) dataset on which the information criterion will be calculated. It usually is the same as DataS.tbc but can be different according to the application (e.g., DataS.tbc is the result of a PCA) or to the goal to reach.}

  \item{init}{Initializing method for the clusters. Six methods are available:

- "block": Blocks initialization (the default)

- "12345": Each day is alternatively allocated to a cluster (For example, if 3 clusters required, day1 goes to C1, day2 to C2, day3 to C3, day4 to C1, day5 to C2, etc.)

- "Kmeans": Initialization by the k-means algorithm

- "Mixtn": Same a "12345" but for length 12 (instead of length 1)

- "EMw": Initialization by the EM clustering algorithm applied onto the w (i.e., large-scale) canonical variates resulting from a CCA performed between DataA.tbc and DataS.tbc

- "EMvw": Same as "EMw" but EM is applied onto both v (local) and w (large-scale) canonical variates.}

  \item{ITmax}{Maximum number of iterations (default is 15) is the algorithm di not converge.}
  \item{rq}{Value (of the local-scale variable of interest) on which the information criterion (IC) is calculated (default is rq=0). rq can be the 90th percentile of the data. In that case, CCM will try to find clusters for which the extremes are well discriminated. A high IC means a good discrimination (in terms of local-scale variable) between the clusters.}


}


\details{For details about the CCM method, see the reference below. 

M. Vrac, P. Yiou. "Weather regimes designed for local precipitation modelling: Application to the Mediterranean basin". JGR-Atmospheres, doi:10.1029/2009JD012871, 2010
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{Info.Criterion}}}
\examples{
## Example
}
\keyword{cluster}
\keyword{models}
