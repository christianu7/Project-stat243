\name{deriv_num}
\alias{deriv_num}
\title{Numeric Differentiation of Functions or Expressions.}
\usage{ deriv_num(fun, x, h=1e-5)}
\description{
  Compute numeric derivatives of functions evaluated at specified values. The formula used for this calculation is as follows, [(f(x)+h)-f(x)]/h, where h is some small number.
}
\arguments{
  \item{fun}{A call of expression with no LHS representing a density}
  \item{x}{ A vector representing a point at which the derivative will be evaluated}  
  \item{h}{ Small scalar value to act as the interval length to compute the numeric differentiation}
}
\details{
  deriv_num is a function used to compute derivatives at given points by estimating the secant lines over very small intervals.  It outputs a numeric value.   The magnitude of h can be changed by the user, but is set to a default of 1E-5.  
}
\value{
   deriv_num returns a numeric value and therefore is helpful in situations where derivative computations are only needed at some coordinates.
}
\references{ W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.}
\examples{
## Normal Distribution
mu <- 5
sigma <- 2
f <- function(x) {dnorm(x,mu,sigma)}
der <- deriv_num(f, 4.5)
## User Defined Function
f <- function(x) { quote(1/(sigma*sqrt(2*pi)) * exp(-(x-mu)^2/(2*sigma^2))) } 
der <- deriv_num(f, 4.5, 1e-3)
}
