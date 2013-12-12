\name{plot.abscissae}
\alias{plot.abscissae}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot upper and lower function and sampling PDF and CDF 
}
\description{
Plot upper and lower function with finite points in abscissae set and also one plot for sampling PDF from \code{\link{s}} function and sampling CDF for \code{\link{S}} 
}
\usage{
plot.abscissae(abscissae, plot.h=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abscissae$h}{
log of objective function that we need to sample. "abscissae" is a list of ( h, T_k, h_T, hp_T, z_i) and its class is "abscissae".
}
  \item{abscissae$T_k}{an ordered vector of values on the support of the log of the objective function to calculate the slope of u in each piecewise segment. }
  \item{abscissae$h_T}{a vector of values of the log of the objective function at values of \eqn{T_k}.}
  \item{abscissae$hp_T}{a vector of the first derivative of the log of the objective function at values in \eqn{T_k}.}
  \item{abscissae$z_i}{a vector of values that give the domain for each piecewise segment of the upper hull.}
    \item{plot.h}{ an argument for users to input whether they want to plot the objective function f that they want to sample.}
}
\details{
It plots lower hull (\code{\link{l}} function), upper hull (\code{\link{u}} function) for the log of objective function and also the sampling pdf (\code{\link{s}} function) and sampling CDF (\code{\link{S}} function). 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
l_f<-100
u_f <- 100
eps <- 0.1
T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
abscissae <- list(T_k=T_k, h_T=h_T, hp_T=hp_T, z_i=z_i)
class(abscissae) <- "abscissae"
plot.abscissae(abscissae, plot.h=F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
