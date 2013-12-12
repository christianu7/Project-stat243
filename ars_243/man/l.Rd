\name{l}
\alias{l}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the squeezing function
}
\description{
\eqn{l} is a squeezing function that gets updated with each iteration of the adaptive rejection sampling algorithm. The squeezing function is a piecewise linear lower hull.
}
\usage{
\eqn{l( x , T_k , h_T )}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of values to act as the argument for l.}
  \item{T_k}{An ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment.}
  \item{h_T}{A vector of values of the log of the objective function at values of \eqn{T_k}.}
  }
\details{
We constructed a linear lower hull by connecting piecewise lines based on every pair of adjacent points in \eqn{T_k}. Each one of the piecewise lines fits two points \eqn{latex}{(x_j, h_T(x_j))} and \eqn{(x_{j+1}, h_T(x_{j+1}))}.  For every pair of the adjacent points \eqn{(x_{j+1}, x_j)} in \eqn{T_k}, the returned value for one x from the squeezing function is \deqn{latex}{\frac{(x_{j+1}-x)h(x_j)+(x-x_j)h(x_{j+1})}{(x_{j+1}-x_j)}}. x is a value between \eqn{x_j} and \eqn{x_{j+1}} and thus it determines Which piece of the lower hull function and which pair of points (x_{j+1}, x_j) to use. If the input x is a vector of many x values, then l function returns a vector of corresponding y values based on the linear lower hull.
}
\value{
\eqn{l} returns a vector of values calculated from the squeezing function corresponding to x.
}
\references{
W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.
}
\author{

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
T_k<-c(0,3,5,6,10)
h_T<-log(dnorm(T_k))
x<-c(-1,2)
l(x=x,T_k=T_k,h_T=h_T)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
