\name{u}
\alias{u}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the envelop function value
}
\description{
u is an envelope function which updates through iterations of the adaptive rejection sampling algorithm. The envelope function is a piecewise linear upper hull to the log of the objective function input by the user.
}
\usage{
\eqn{u( x , T_k , h_T , hp_T , z_i )}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of values to act as the argument for u.}
  \item{T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment.}
  \item{h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
  \item{hp_T}{A vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{z_i}{A vector of values that give the domain for each piecewise segment of the upper hull.}
  }
\details{
We constructed a linear upper hull for the  log of the objective function by connecting piecewise tangent lines based on each points in \eqn{T_k}. That is, each one of the piecewise lines is the tangent line at a point \eqn{x_j}, whose slope is h_T_j. The transition points of the tangent lines forms the ordered set \eqn{z_i}. If the value of x falls into the range of \eqn{(z_{j-1}, z_j)} in the \eqn{z_i} set, the returned value for x from the envelop function is \eqn{h(x_j)+(x-x_j)h'(x_j)=h_T_j+(x-x_j)hp_T_j}. The x should be a value between \eqn{z_{j-1}} and \eqn{z_j} and thus its value determines which piece of the upper hull function and which point \eqn{x_j} in \eqn{T_k} set to use. For those values larger than the last one, the last value \eqn{x_{length(z_i)}} is used as \eqn{x_j} in the corresponding piece of envelope function. 
}
\value{
A vector of values of the envelope function evaluated at values of x. 
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
u( x=x , T_k=T_k , h_T=h_T , hp_T=hp_T , z_i=z_i )
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
