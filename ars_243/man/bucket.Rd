\name{bucket}
\alias{ bucket}
\title{Bucket Function.}
\usage{bucket(x, x_i)}
\description{
    Group a given value x into pre-determined ranges defined by x_i, and return the index of which bucket x belongs to.
  }
\arguments{
  \item{ x}{ A numeric value to be tested and sorted into a bucket}
  \item{ x_i}{ A vector of values representing starting points of the various buckets}  
}
\details{
   Bucket is used to assign the given x to its associated group or range of values.  This is done by utilizing a logical statement to determine if x is greater than or less than each element of x_i.  The index of the proper bucket is then returned as output.
}
\value{
   The bucket function returns the index of the bucket in which the user-input x belongs.  
}
\references{ W. R. Gilks and P. Wild. Adaptive Rejection Sampling for Gibbs Sampling. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 41, No. 2(1992), pp. 337-348.}
\examples{
   myBuckets <- c(1, 3, 6, 9)
   bucket(4, myBuckets)
}
