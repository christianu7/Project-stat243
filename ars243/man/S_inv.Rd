\name{S_inv}
\alias{S_inv}

\title{
Inverse of the Cumulative Distribution of the exponentiated envelope function
}
\description{
Given a vector of values between 0 and 1, \eqn{S_inv} will give a sample that come from the sampling distribution \eqn{S}.
}
\usage{
\eqn{S_inv(x , T_k, h_T , hp_T , z_i)} 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of values between 0 and 1.
}
  \item{abscissae$T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment. "abscissae" is a list of ( T_k, h_T, hp_T, z_i) and its class is "abscissae".}
   \item{abscissae$h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
 \item{abscissae$hp_T}{A vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{abscissae$z_i}{A vector of values that give the domain for each piecewise segment of the upper hull.}
}
\details{
\eqn{S_{inv}} is the inverse of \code{\link{S}}. The function has particular form of exponential of piecewise linear function. Given a value between 0 and 1, it can be determined which piece of the exponentiated envelope function to use by \code{\link{bucket}} (denoted by j). After subtracting the cumulative density over the preceding (j-1) spaces, apply the remainder to the corresponding piece of inverse function and then obtain the data point for the X variable.  
}
\value{
A vector of points whose CDF is \eqn{S}.
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
abscissae <- list(T_k=T_k, h_T=h_T, hp_T=hp_T, z_i=z_i)
class(abscissae) <- "abscissae"
x<-c(0.5,0.95)
S_inv(x , abscissae)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
