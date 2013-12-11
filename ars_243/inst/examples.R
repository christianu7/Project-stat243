rm(list=ls())

script.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/ars_243/R/"
out.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/"

setwd(script.dir)
source("auxiliar_fun.R")
source("abscissae_class.R")
source("abscissae_methods.R")
source("ars.R")

setwd(out.dir)

B <- 1000
eps=1e-07

if(T) {
  # Normal distribution
  mu <- 5
  sigma <- 2
  #f <- expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) )
  f <- function(x) {dnorm(x,mu,sigma)}
  l_f=mu-10*sigma
  u_f=mu+10*sigma
}

if(F) {
  # Beta distribution
  a<-5
  b<-2
  f <- function(x) {dbeta(x,a,b)}
  l_f=0
  u_f=1
}

if(F) {
  # Gamma distribution
  r=15
  lambda=1
  f <- function(x) {dgamma(x,r,lambda)}
  l_f=0
  u_f=75
}

if(F) {
  df <- 10
  f <- function(x) { dchisq(x, df) }
  l_f=0
  u_f=50
}

ars( B, f ,l_f, u_f,
    init_abs=seq(l_f+0.01,u_f-0.01,length.out=4),
    ep=1e-10,
    m=10,
    evol.gif=F
    )
  
  