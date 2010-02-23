\name{Info.Criterion}
\title{Computes the Information Criterion (IC) of a clustering result, for values higher than r.}
\usage{
Info.Criterion(NS, DataS, r, totCL, Nc, cl)
}
\alias{Info.Criterion}
\description{
This function computes the Information Criterion (IC) of a clustering result.
}
\arguments{
  \item{NS}{Number of locations (i.e., weather stations) for the local-scale time series on which IC is calculated.}
  \item{DataS}{Dataset corresponding to local-scale (station) data on which IC is calculated. This is a matrix NS*NN, where NN is the number of days (i.e., length of the time series).}
  \item{r}{Value for which the IC is calculated (see details).}
  \item{totCL}{Vector of numbers of elements (e.g., days) in each cluster.}
  \item{Nc}{Number of clusters.}
  \item{cl}{Vector containing the sequence of clusters (length(cl) is NN).}
}
\details{The IC is computed as $IC = Sum_{i=1}^K |n_{i,r} - (p_r * n_i)|$, where

$n_{i,r}$ = \# of days in cluster $i$ that receive a rainfall amount > r

$p_r$ = proba of such rainy days in the whole population

$n_i$ = \# of days in cluster i
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{CCM}}}
\examples{
## Example
}
\keyword{cluster}
