\name{Percent.bad.and.false.classif.per.cluster}
\title{Percentage of bad and false classification}
\usage{
Percent.bad.and.false.classif.per.cluster(cl, new.cl)
}
\alias{Percent.bad.and.false.classif.per.cluster}
\description{
This function computes the percentage of bad and false classification of a sequence of clusters (new.cl) according to a reference sequence (cl).
}
\arguments{
  \item{cl}{Reference sequence of clusters.}
  \item{new.cl}{Sequence of clusters to be compared to the reference sequence.}
}

\value{Returns a list containing the following elements:
  \item{tot}{Global percentage of bad classification}
  \item{BadPerCluster}{Percentage of bad classification per cluster}
  \item{FalsePerCluster}{Percentage of false classification per cluster}
  \item{mat.att}{Global matrix of attribution (row = cl, colomn = new.cl)}
}

\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{learn.and.project.clusters}}}
\examples{
## Example
}
\keyword{cluster}

