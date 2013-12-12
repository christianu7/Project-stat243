\name{permdiff}
\alias{permdiff}

\title{
Permutation Test (Non-Parametric Test)
}
\description{
Shows the difference between the ARS sample distribution and the theoretical distribution to evaluate accuracy of the implemented ARS algorithm.
}
\usage{
permdiff(x, y, B)
}

\arguments{
  \item{x}{Sample 1}
  \item{y}{Sample 2}
  \item{B}{Number of simulations to be tested}
}
\details{
The permutation test function interchanges points of x and y distributions B times and keeps track of the difference of means of each permutation.  P values are then output, which are the number of simulated differences greater than the true difference in means between x and y.  Small p values imply that distribution x and distribution y are not the same.
}
\examples{
x <- rnorm(30)
y <- runif(30, min=-1, max=1)
permdiff(x=x, y=y, 100)
}

