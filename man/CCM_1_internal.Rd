\name{CCM_1_internal}
\title{This function is for internal use and is not meant to be called by the user.}
\usage{
CCM_1_internal(Nc, NS, DataA.tbc,DataS.tbc,NN, DataStation, init="block")
}
\alias{CCM_1_internal}
\description{
This function is for internal use and is not meant to be called by the user.
It performs the CCM algorithm.
}
\arguments{

  \item{Nc}{Same as the Nc argument of CCM.}
  \item{NS}{Same as the NS argument of CCM.}
  \item{DataA.tbc}{Same as the DataA.tbc argument of CCM.}
  \item{DataS.tbc}{Same as the DataS.tbc argument of CCM.}
  \item{NN}{Same as the NN argument of CCM.}
  \item{DataStation}{Same as the DataStation argument of CCM.}
  \item{init}{Same as the init argument of CCM.}
}


\details{For details about the CCM method, see the reference below. 

M. Vrac, P. Yiou. "Weather regimes designed for local precipitation modelling: Application to the Mediterranean bassin". Submitted, JGR-A, 2009
}
\author{M. Vrac (mathieu.vrac@lsce.ipsl.fr))}
\seealso{\code{\link{CCM}}}
\examples{
## Example
}
\keyword{internal}