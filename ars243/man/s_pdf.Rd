\name{s}
\alias{s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized exponentiated envelope function.
}
\description{
\eqn{s} is the sampling PDF found by exponentiating of the envelope function calculated from \eqn{u} and normalizing it using \eqn{int_s}.
}
\usage{
\eqn{s(x , T_k, h_T , hp_T , z_i)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of values to determine the density of \eqn{s} at.
}
  \item{T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment.}
  \item{h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
  \item{hp_T}{A vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{z_i}{A vector of values that give the domain for each piecewise segment of the upper hull.}
}
\details{
First exponentiate the upper function calculated from \code{\link{u}}. To make it a density, it is normalized by dividing by \eqn{int_s}.  
}
\value{
A vector of density values of the normalized exponentiated envelope function for values of x.
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
s( x=x , T_k=T_k , h_T=h_T , hp_T=hp_T , z_i=z_i )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
