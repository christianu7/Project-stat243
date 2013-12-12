\name{ars}
\alias{ars}
\title{Adaptive Rejection Sampling}
\description{

}
\usage{
ars(B, f, l_f=-Inf, u_f=Inf, init_abs=NULL, ep=1e-10 , m=10, evol.pdf=F)
}
\arguments{
  \item{B}{Total number of points to be sampled using ARS}
  \item{f}{A function to be sampled from}
  \item{l_f}{Lower bound of the function f}
  \item{u_f}{Upper bound of the function f}
%%  \item{init_abs}{} function to get initial values x if user does not input them...not coded yet
  \item{ep}{Small scalar value to act as the interval length to compute the numeric differentiation}
  \item{m}{Single value to determine how many samples to draw in each iteration of the algorithm}
  \item{evol.pdf}{A character string: If "NO", nothing will happen, if listed as some file name, live plotting will take place as the code is running and will be saved in that name/location}
}
\details{
The ars function first checks that the user input function f is of the class 'function'.  If it is not, it automatically converts expressions to function form before proceeding, or else returns an error for the user.  Initial abscissae for the algorithm are then validated using the \code{\link{as.abscissae}} function.  A check is then used to run additional tests on the user inputs, (i.e. log-concavity).  

The simulation is then run until the desired number of points have been sampled from the user's function.  The simulation begins with sampling from a random uniform over [0,1].  X* is then found by sampling from s(x) (See help for \code{\link{S_inv}}, \code{\link{S}}, \code{\link{s}}, and \code{\link{int_s}}).

A plot is then generated to show that the user's function f(x) is the same as the ars function's s(x).  After this check, exp( h(x) - UB_function(x) ) and exp( LB_function(x) - UB_function(x) ) are plotted to depict the acceptance and rejection areas.  The lower bound function and upper bound function are defined methods named 'l' and 'u' respectively (see \code{\link{l}} and \code{\link{u}}.)  The X* sampled points are then plotted on top of this existing visual so it can be observed if they will be accepted or rejected.

The formal squeeze and rejections tests are performed by comparing the sampled uniform, w, to exp( x* - u(x*) ), where u(x) is the upper bound function \code{\link{u}}.  If w is less than this value, then the sampled point x* is accepted.  Any points that are not accepted at this stage are passed through the rejection test, where h(x) and h'(x) are evaluated at those rejected x* values.  Uniform w's are again compared to exp( x* - u(x*) ), with the \code{\link{u}} function reevaluated for the new h(x) and h'(x) information.   From the squeeze and rejection tests, all accepted points are added to the set of simulated values, and the x values are added to the abscissae list which is then reordered along with the associated h(x) and h'(x).  The next iteration then begins with this information.

If the argument named evol.pdf is listed as a file directory, then the simulation will be paired with a live visualization of the plotting of sampled points. Otherwise, the argument is defaulted to "NO" and will not print the plots.
}
\value{
The ars function returns a vector of the B simulated values found.
}
\references{
 W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.}}
\examples{
f<- function(x){dnorm(x,5,2.5)} 
ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='NO')  #no pdf displayed

ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='test.pdf')  #pdf saved to working directory

ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='C:\\Users\\User\\Documents\\test.pdf')  #pdf saved to specific directory
}

