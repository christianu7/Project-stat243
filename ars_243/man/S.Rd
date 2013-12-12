\name{S}
\alias{S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cumulative density function of normalized exponentiated upper hull function
}
\description{
\eqn{S} evaluates the CDF of the exponentiated upper hull function calculated from \eqn{u} at values of x.
}
\usage{
\eqn{S(x , T_k, h_T , hp_T , z_i)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of values where the CDF is evaluated.
}
  \item{T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment.}
  \item{h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
  \item{hp_T}{A vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{z_i}{A vector of values that give the domain for each piecewise segment of the upper hull.}}
}
\details{
\eqn{S} evaluates the cumulative density of the exponentiated upper hull function. First, determine which segment of the exponentiated envelope function x falls into, say the j-th. Add the integral on all the (j-1) segments before the j-th segment. Then integrate the j-th piece of the function over the interval \eqn{[z_{j-1}, x]} and add it to the preceding integral to get the CDF of the exponentiated envelope function at x.  
}
\value{
A vector of  values of the CDF of the exponentiated envelope function for the corresponding x values.
}
\references{
W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2 (1992), pp. 337-348.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
x<-c(-1,3)
S(x , T_k, h_T , hp_T , z_i)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
