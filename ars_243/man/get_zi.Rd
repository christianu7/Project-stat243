\name{get_zi}
\alias{ get_zi}
\title{Compute Intersection Points of Derivatives.}
\usage{ get_zi(x)}
\description{
   This function calculates the x values of intersecting points of the function derivative at specified points.  
}
\arguments{
  \item{ x}{ A list of X values, function evaluations and derivative evaluations at those points}
}
\details{
   The get_zi function uses these inputs to generate intersection points using the following formula:
( h_T[-1] - h_T[-k] - T_k[-1] * hp_T[-1] + T_k[-k] * hp_T[-k] ) / (hp_T[-k]-hp_T[-1])
}
\value{
   The lower bound, z values and upper bound are returned from the get_zi function.  Additionally, the z_i values are input to the summary information provided by the \code{\link{create_abscissae}} function.
}
\references{ W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.}
\examples{
testInput <- list(T_k=c(-10,1,4,8), h_T=c(-19, -3, -2, -2.5), hp_T=c(2, 0.5, 0.1, -0.5), z_i=NULL, k=4, l_h=-12, u_h=10)
get_zi.abscissae(testInput)