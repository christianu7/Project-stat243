rm(list=ls())

script.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/Project-stat243/ars_243/R/"
out.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/"

setwd(script.dir)
source("auxiliar_fun.R")
source("abscissae_class.R")
source("abscissae_methods.R")
source("ars.R")
setwd(out.dir)

#####     EXAMPLES WITH DISTRIBUTIONS     #####

# Normal distribution
set.seed(0)
mu <- 5
sigma <- 2
ars( B=1000,
     #f=function(x) {dnorm(x,mu,sigma)},
     f=expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) ),
     l_f=-Inf,
     u_f=Inf,
     init_abs=seq(-10,10,5),
     eps=1e-5,
     m="exp",
     rej_evol.pdf= "rej_normal.pdf",
     abs_evol.pdf= "abs_normal.pdf",
     hist.pdf = "hist_normal.pdf"
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
     eps=1e-5,
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
     eps=1e-2,
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
     #f=function(x) {dgamma(x,shape=r,rate=2)},
     f=expression( (lambda^r)/gamma(r) * x^(r-1) * exp(- lambda * x) ),
     l_f=0,
     u_f=Inf,
     init_abs=seq(1,5,1),
     eps=1e-5,
     m="exp",
     rej_evol.pdf= "rej_gamma.pdf",
     abs_evol.pdf= "abs_gamma.pdf",
     hist.pdf = "hist_gamma.pdf"
)
