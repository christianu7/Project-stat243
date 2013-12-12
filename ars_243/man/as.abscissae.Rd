\name{as.abscissae}
\alias{as.abscissae}
\title{ Generating Output of Summary Information.}
\usage{ as.abscissae(x, h, l_h, u_h, eps)) }
\description{
  Supporting function to create an output of summary information regarding the abscissae input by user and checked by \code{\link{check.abscissae}}.  
}
\arguments{
  \item{x}{ vector of initial points }
  \item{h}{ The function (call) being used for analysis }  
  \item{l_h}{ The lower bound of the function}
  \item{u_h}{ The upper bound of the function}
  \item{ep}{ The differential between each point of x and its next evaluation for estimation of the derivative}
}
\details{
  as.abscissae is used to combine information into a clean output to be used as an output of the initVal function.  The returned value is a list of all key points needed for the adaptive rejection sampling analysis.
}
\value{
   as.abscissae returns a list of the initial X values, the function h evaluated at each of those points, the derivative evaluated at those points, the number of X values (k), and the lower and upper bounds of the function.
}
\examples{
   ## Generic data generation
l_f <- 1e-5
u_f <- 1e5
h <- function(x){dnorm(mu,sigma)}
abscissae <- as.abscissae(x=seq(l_f+0.01,u_f-0.01,length.out=4), h, l_h=l_f, u_h=u_f, ep=1e-8)
}

