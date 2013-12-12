\name{check.abscissae}
\alias{check.abscissae }
\title{Generation of Initial X Values for ARS.}
\usage{check.abscissae(x, bound, ep)}
\description{
   The check.abscissae function takes two or more user-input points  as well as upper and lower bounds and adjusts the points to create effective initial x values for the adaptive rejection sampling algorithm.  
  }
\arguments{
  \item{ x}{ a vector of at least two x values within the lower and upper bounds (in ascending order)}
  \item{bound}{ A vector of (lower bound, upper bound)}  
  \item{ ep}{ The differential between each point of x and its next evaluation for estimation of the derivative}
}
\details{
   The inputs passed through check.abscissae go through a series of checks to ensure that the points are valid starting x values for the adaptive rejection sampling algorithm.  Error messages are returned if the length of x is less than two or if any elements of x are not unique.  A warning is returned if the user inputs a lower bound that is larger than the upper bound.  The next check returns an error if the provided x values are not within the listed bounds.  The values of x must have mass in f or else the algorithm will be unable to proceed because h=log(f) will not be finite.  If this is not the case, those values will be discarded; a warning will be returned if values are discarded until there are less than two points (as this fails the first check mentioned.)  Final checks are performed to ensure that there are x values on either side of the global optimum.   Warnings lead to automated changes to the user inputs, and errors break the function and prompt the user to input different parameters.   After all conditions are met, check.abscissae returns the finalized x values (T_k), the corresponding h(x) and h'(x) values, the length of the x vector (k), and the upper and lower bounds.  
}
\value{
   The returned value of check.abscissae is a list of the x values and the associated evaluations of the function h and the derivative of h.  The list also includes summary outputs for k, lower bound and upper bound.  This list of components can then be used as a single summary object as an input to future functions in the adaptive rejection sampling algorithm.
   \item{T_k}{validated starting x values}
   \item{h_T}{h(x) evaluations at the values in T_k}
   \item{hp_T}{h'(x) evaluations}
   \item{z_i}{NULL (to be filled by another function)}
   \item{l_h}{lower bound of h(x) input by user}
   \item{u_h}{upper bound of h(x) input by user}
}
\references{ W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.}
\examples{
 f <- function(x) { dnorm(x, 5, 2.5) }
	h <- function(x) { log(f(x)) }
	x <- c(-10, 1, 4, 8)
	check.abscissae(x=x, h=h, l_h=-12, u_h=10, ep=1e-3)
}
