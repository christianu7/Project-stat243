\name{add_points.abscissae}
\alias{add_points.abscissae}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update the abscissae set
}
\description{
Add new points to the original abscissae set and then re-order the T_k set (an element in object abscissae) from small and change the order in abscissae$h_T and abscissae$h_T accordingly.
}
\usage{
add_points.abscissae(x,new_T_k,new_h_T,new_hp_T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 the old abscissae set which is a list(T_k, h_T, hp_T, z_i, k). T_k is An ordered vector of values on the support of the log of the objective function. h_T is a vector of values of the log of the objective function at values of T_k. abscissae$hp_T is a vector of the first derivative of the log of the objective function at values in T_k. A vector of values that implies the domain for each piecewise segment of the upper hull. k is the length of T_k set. 
}
  \item{new_T_k}{
 a vector of values which needs to be added to the x$T_k
}
  \item{new_h_T}{
 a vector of values which needs to be added to the x$h_T
}
  \item{new_hp_T}{
 a vector of values which needs to be added to the x$hp_T
}
}
\details{
 combine new_T_k to x$T_k to make a new T_k set and reorder this new T_k set. Then add new_h_T to x$h_T and add new_hp_T to x$hp_T to make new h_T set and hp_T set. Order these two new sets according to the order in T_k set. z_i is the supporting value for the transition point in upper hull and its default is NULL. It can be calculated in \code{\link{get_zi.abscissae}} function. The calculate the number in T_k set to be the value of k.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
x <- list(T_k=T_k, h_T=h_T, hp_T=hp_T)
class(x) <- "abscissae"
new_T_k <- c(-5,5)
new_h_T<-log(dnorm(new_T_k))
hp<-function(x){-x}
new_hp_T<-hp(new_T_k)
add_points.abscissae(x = x, new_T_k = new_T_k, new_h_T = new_h_T, new_hp_T = new_hp_T)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
