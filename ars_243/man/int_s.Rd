\name{int_s}
\alias{int_s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Integral of exponentiated upper hull function
}
\description{
\eqn{int_s} is to exponentiate each piece of the upper hull and then integrate them to normalize \eqn{s}. 
}
\usage{
\eqn{int_s(T_k, h_T, hp_T, z_i)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment.}
   \item{h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
  \item{hp_T}{A vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{z_i}{A vector of values that give the domain for each piecewise segment of the upper hull.}}
\details{
First calculate the piecewise linear functions from \code{\link{u}} function and integrate them with the corresponding boundary. Each piece of the integral is \eqn{latex}{1/h'(x_j)*exp(u(z_{j+1}-u(z_j)))}. \eqn{int_s} will be the sum of all these pieces of integral and serve as a normalizing component for the sampling density .  

}
\value{
A value that is the integral of the exponentiated upper hull function.
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
int_s(T_k, h_T, hp_T, z_i)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
