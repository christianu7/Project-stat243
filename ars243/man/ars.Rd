\name{ars}
\alias{ars}
\title{Adaptive Rejection Sampling}
\description{

}
\usage{
	ars( B = 100, f, l_f = -Inf, u_f = Inf, init_abs, eps = 1e-04, m = "exp",
    	rej_evol.pdf = NULL, abs_evol.pdf = NULL, hist.pdf = NULL
    	)
}
\arguments{
  \item{B}{Total number of points to be sampled using ARS}
  \item{f}{The function to be sampled from, it has to be an object from class "expression" or "function"}
  \item{l_f}{Lower bound of the function f}
  \item{u_f}{Upper bound of the function f}
  \item{init_abs}{ Initial values for abscissaes to run ARS }
  \item{ep}{Small scalar value to act as the interval length to compute the numeric differentiation}
  \item{m}{Determine how many samples to draw in each iteration of the algorithm.
  			It can be a numeric value or a string: "exp" for sampling 2^i values in iteration i, or "lin" for sampling 2*i values in iteration i}
  \item{rej_evol.pdf}{A character string: specifies the file to save a pdf with the evolution of the rejection areas for each iteration.
  				If evol.pdf=NULL (default), no file is produced}
  \item{abs_evol.pdf}{A character string: specifies the file to save a pdf with the evolution of the functions u(), l() s() and S() for each iteration.
  				If abs_evol.pdf=NULL (default), no file is produced}
  \item{hist.pdf}{A character string: specifies the file to save a pdf with the histogram of the final sample.
  				If hist.pdf=NULL (default), no file is produced}
}
\details{
The ars function first checks that the user input function f is of the class 'function'.  If it is not, it automatically converts expressions to function form before proceeding, or else returns an error for the user.  Initial abscissae for the algorithm are then validated using the \code{\link{as.abscissae}} function.  A check is then used to run additional tests on the user inputs, (i.e. log-concavity).  

The simulation is then run until the desired number of points have been sampled from the user's function.  The simulation begins with sampling from a random uniform over [0,1].  X* is then found by sampling from s(x) (See help for \code{\link{S_inv}}, \code{\link{S}}, \code{\link{s}}, and \code{\link{int_s}}).

The lower bound function and upper bound function are defined methods named 'l' and 'u' respectively (see \code{\link{l}} and \code{\link{u}}.)

The formal squeeze and rejections tests are performed by comparing the sampled uniform, w, to exp( x* - u(x*) ), where u(x) is the upper bound function \code{\link{u}}.  If w is less than this value, then the sampled point x* is accepted.  Any points that are not accepted at this stage are passed through the rejection test, where h(x) and h'(x) are evaluated at those rejected x* values.  Uniform w's are again compared to exp( x* - u(x*) ), with the \code{\link{u}} function reevaluated for the new h(x) and h'(x) information.   From the squeeze and rejection tests, all accepted points are added to the set of simulated values, and the x values are added to the abscissae list which is then reordered along with the associated h(x) and h'(x).  The next iteration then begins with this information.
}
\value{
The ars function returns a vector with the B simulated values from f.
Aditionally, it can create up to 3 pdf files with the evolution of the method.
}
\references{
 W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.
 }
\examples{

#####     EXAMPLES WITH DIFFERENT DISTRIBUTIONS     #####

# Normal distribution
set.seed(0)
mu <- 5
sigma <- 2
ars( B=1000,
     #f=function(x) {dnorm(x,mu,sigma)},
     f=expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) ),
     l_f=-Inf,
     u_f=Inf,
     init_abs=seq(0,6,1),
     m="exp",
     rej_evol.pdf= "rej_normal.pdf",
     abs_evol.pdf= "abs_normal.pdf",
     hist.pdf = "hist_normal.pdf"
)

# Truncated normal distribution
set.seed(0)
mu <- 5
sigma <- 2
ars( B=1000,
     #f=function(x) {dnorm(x,mu,sigma)},
     f=expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) ),
     l_f=-2,
     u_f=7,
     init_abs=seq(3,6,1),
     m="exp",
     rej_evol.pdf= "rej_normal_trunc.pdf",
     abs_evol.pdf= "abs_normal_trunc.pdf",
     hist.pdf = "hist_normal_trunc.pdf"
)


# Beta distribution
set.seed(0)
a <- 2
b <- 2
ars( B=1000,
     f=function(x) {dbeta(x,a,b)},
     #f=expression( (gamma(a+b)/(gamma(a)*gamma(b))) * x^(a-1) * (1-x)^(b-1) ),
     l_f=0,
     u_f=1,
     init_abs=seq(0.1,0.9,0.2),
     m="exp",
     rej_evol.pdf= "rej_beta.pdf",
     abs_evol.pdf= "abs_beta.pdf",
     hist.pdf = "hist_beta.pdf"
)


# Exponential distribution
set.seed(0)
lambda <- 2
ars( B=1000,
     f=function(x) {dexp(x,lambda)},
     #f=expression( lambda * exp(- lambda * x) ),
     l_f=0,
     u_f=Inf,
     init_abs=seq(1,5,1),
     m="exp",
     rej_evol.pdf= "rej_exp.pdf",
     abs_evol.pdf= "abs_exp.pdf",
     hist.pdf = "hist_exp.pdf"
)



# Gamma distribution
set.seed(0)
r <- 5
lambda <- 2
ars( B=1000,
     #f=function(x) {dgamma(x,shape=r,rate=lambda)},
     f=expression( (lambda^r)/gamma(r) * x^(r-1) * exp(- lambda * x) ),
     l_f=0,
     u_f=Inf,
     init_abs=seq(1,5,1),
     m="exp",
     rej_evol.pdf= "rej_gamma.pdf",
     abs_evol.pdf= "abs_gamma.pdf",
     hist.pdf = "hist_gamma.pdf"
)


}

