rm(list=ls())

if(F) {
  script.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/Project-stat243/ars_243/R/"
  out.dir <- "/Users/Chris/Documents/26 UC Berkeley/03 Courses/STAT 243/Final Project/"
}

setwd(script.dir)
source("auxiliar_fun.R")
source("abscissae_class.R")
source("abscissae_methods.R")
source("ars.R")

setwd(out.dir)

if(F){
  install.packages('VGAM')
  require('VGAM')
}

B <- 1000
ep=1e-5

if(F) {
  # Normal distribution
  mu <- 5
  sigma <- 2
  #f <- expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) )
  f <- function(x) {dnorm(x,mu,sigma)}
  l_f = -Inf #mu - 10*sigma
  u_f = Inf #mu + 10*sigma
  init_val <- c(-10, 3, 4, 5, 6, 15)
  test_N <- test(B, f, init_val, l_f, u_f, rnorm, mean = mu, sd = sigma, ep = ep)
  test_N$Means
  test_N$`p-value`
}
if(F) {
  # Beta distribution
  a <- 5
  b <- 2
  f <- function(x) {dbeta(x,a,b)}
  l_f = 0
  u_f = 1
  init_val <- c(.4, .6, .9)
  test_B <- test(B, f, init_val, l_f, u_f, rbeta, shape1 = a, shape2 = b, ep = ep)
  test_B$Means
  test_B$`p-value`
}
if(F) {
  # Gamma distribution
  r=15
  lambda=1
  f <- function(x) {dgamma(x,r,lambda)}
  l_f = 0
  u_f = Inf
  init_val <- c(5, 10, 15, 20, 25, 40)
  test_Gm <- test(B, f, init_val, l_f, u_f, rgamma, shape = r, rate = lambda, ep = ep)
  test_Gm$Means
  test_Gm$`p-value`
}
if(F) {
  # Chi-Square distribution
  df <- 10
  f <- function(x) { dchisq(x, df) }
  l_f = 0
  u_f = Inf
  init_val <- c(3, 5, 10, 15, 20, 25)
  test_Ch <- test(B, f, init_val, l_f, u_f, rchisq, df = df, ep = ep)
  test_Ch$Means
  test_Ch$`p-value`
}
if(F) {
  # Exponential distribution
  lambda <- .1
  f <- function(x) { dexp(x, lambda)}
  l_f = 0
  u_f = Inf
  init_val <- c(1.5, 2, 2.5, 3, 10, 20, 40)
  test_E <- test(B, f, init_val, l_f, u_f, rexp, rate = lambda, ep = ep)
  test_E$Means
  test_E$`p-value`
  #x's must be fairly close to the mean of this distribution
  #to get reasonable results. 
}
if(F) {
  # t distribution
  df<- 20
  f <- function(x) { dt(x, df)}
  l_f = -Inf
  u_f = Inf
  init_val <- c(-10, -5, 0.75, 1, 1.5, 3, 5, 7, 10)
  test_T <- test(B, f, init_val, l_f, u_f, rt, df = df, ep = ep)
  test_T$Means
  test_T$`p-value`
  #The degrees of freedom must be larger than the absolute value 
  #of the maximum and minimum of the x's.
}
if(F){
  # Gumbel distribution
  mu <- 10
  b <- 5
  f <- function(x){ dgumbel(x, mu, b)}
  l_f <- -Inf
  u_f <- Inf
  init_val <- c(0, 7, 10, 11, 12, 15, 30)
  test_G <- test(B, f, init_val, l_f, u_f, rgumbel, location = mu, scale = b, ep = ep)
  test_G$Means
  test_G$`p-value`
}
if(F){
  # Weibull distribution
  a <- 5
  b <- 5
  f <- function(x){ dweibull(x, a, b)}
  l_f <- 0
  u_f <- Inf
  init_val <- c(1, 3.5, 4.2, 4.5, 4.8, 6, 10)
  test_W <- test(B, f, init_val, l_f, u_f, rweibull, shape = a, scale = b, ep = ep)
  test_W$Means
  test_W$`p-value`
  #x values must be centered around mean with a somewhat wide 
  #spread to get a somewhat accurate mean.
}
if(F){
  # Pareto distribution
  a <- 1
  b <- 3
  f <- function(x){ dpareto(x, a, b)}
  l_f <- 1.0001
  u_f <- Inf
  init_val <- c(1.1, 1.2, 1.3, 1.5, 1.9, 2.5)
  test_P <- test(B, f, init_val, l_f, u_f, rpareto, location = a, shape = b, ep = ep)
  test_P$Means
  test_P$`p-value`
}
if(F){
  # F distribution
  df1 <- 3
  df2 <- 5
  f <- function(x){ df(x, df1, df2)}
  l_f <- 0
  u_f <- 50
  init_val <- c(0.01, 0.2, 0.3, 0.5, 0.6, 5, 20, 30, 40)
  test_F <- test(B, f, init_val, l_f, u_f, rf, df1 = df1, df2 = df2, ep = ep)
  test_F$Means
  test_F$`p-value`
}
if(F){
  # 1/(x^5)
  f <- function(x){ x^(-5)}
  l_f <- 0.01
  u_f <- Inf
  init_val <- c(1.5, 2, 3)
  test_poly <- ars(B, f, init_val, l_f, u_f, ep = 1e-3)
  mean(test_poly)
  hist(test_poly, breaks = 75, main = "1/(x^5)")
}
if(F){
  # sin(x)
  a <- 100
  f <- function(x){ a*sin(x)}
  l_f <- pi/16
  u_f <- 15*pi/16
  init_val <- c(0.5, 0.9, 1.5, 2)
  test_sin <- ars(B, f, init_val, l_f, u_f, ep = 1e-3)
  mean(test_sin)
  hist(test_sin, breaks = 100, main = "sin(x)")
}



