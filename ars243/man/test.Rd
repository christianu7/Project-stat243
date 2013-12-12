\name{test}
\alias{test}

\title{
Testing Function
}
\description{
Samples from two distributions (generally one heuristically found and the other theoretical.)  Summary statistics are returned comparing the two.
}
\usage{
test(B, f, init_val, l_f, u_f, rdensity, ddensity, ep = 1e-3, leg="Density", ...)
}

\arguments{
  \item{B}{number of sampled points from each of the distributions}
  \item{f}{function to generate sample from (the 'unknown')}
  \item{init_val}{initial points for the ARS algorithm}
  \item{l_f}{lower bound for ARS}
  \item{u_f}{upper bound for ARS}
  \item{rdensity}{R function for distribution sampling (i.e. rnorm)}
  \item{ddensity}{R function for distribution density (i.e. dnorm)}
  \item{ep}{small value used for calculating numeric derivatives}
  \item{leg}{legend for plot}
  \item{...}{to be used for parameters to be passed into rdensity and ddensity}
}
\details{
Samples are drawn from the two input distributions.  Means of each distribution are determined, and then the \code{\link{permdiff}} function is called to calculate p values. A histogram is plotted of the 'unknown' distribution, and that plot is overlaid with the theoretical distribution's curve.  
}
\value{
Test returns a list of summary statistics including:
  \item{samp_ars}{the points sampled from the ARS distribution}
  \item{samp_theo}{the poinds sampled from the theoretical distribution (ddensity)}
  \item{mean}{a 2-element vector giving the means of both distributions}
  \item{p}{the p value returned by the permdiff function}
}

\seealso{
\code{\link{permdiff}}
}
\examples{

}

