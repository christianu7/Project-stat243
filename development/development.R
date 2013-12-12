###########Development##############
####as.abscissae situations#########

#The following is a list of situations to test 
#as.abscissae against to verify the return of 
#error messages in the appropriate places

if(F){h <- function(x){log(dnorm(x))}}
if(F){h <- function(x){log(dnorm(x, 0, 100))}}
if(F){h <- function(x){log(dchisq(x,10))}}
if(F){h <- function(x){log(dunif(x, 0, 100))}}

if(F){bound <- c(-Inf, Inf)}
if(F){bound <- c(0, Inf)}
if(F){bound <- c(0, 100)}
if(F){bound <- c(0, Inf)}
if(F){bound <- c(-100, 1000)}
if(F){bound <- c(-100, 10)}
if(F){bound <- c(10, 1000)}

if(F){init_val <- c(-100, 0, 100)}
if(F){init_val <- c(0, 0, 100)}
if(F){init_val <- c(0, 1, 5, 15, 100)}
if(F){init_val <- c(1)}
if(F){init_val <- c(10, -100, 1000)}
if(F){init_val <- c(-1000, -100, 1000)}
if(F){init_val <- c( 1, 5, 10, 11, 12, 13, 15, 35)}
if(F){init_val <- c(1, 2, 3, 4, 5)}

#####################################