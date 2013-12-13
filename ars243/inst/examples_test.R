#rm(list=ls())
require('VGAM')

B<-1000

  # Normal distribution
  mu <- 5
  sigma <- 2
  test_Normal <- test(
      B,
      f=expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) ),
      #f=function(x) {dnorm(x,mu,sigma)},
      init_val=c(-10, 3, 4, 6, 15),
      l_f=-Inf,
      u_f=Inf,
      rdensity=rnorm,
      ddensity=dnorm,
      mean = mu, sd = sigma,
      leg = "Normal"
  )


  # Beta distribution
  a <- 5
  b <- 2
  test_B <- test(
      B,
      f=function(x) {dbeta(x,a,b)},
      init_val=c(.4, .6, .9),
      l_f=0,
      u_f=1,
      rbeta,
      dbeta,
      shape1 = a,
      shape2 = b,
      ep = ep,
      leg = "Beta"
    )

  # Gamma distribution
  r=15
  lambda=1
  init_val <- 
  test_Gm <- test(
    B,
    f=function(x) {dgamma(x,r,lambda)},
    init_val=c(5, 10, 15, 20, 25, 40),
    l_f=0,
    u_f=Inf,
    rgamma,
    dgamma,
    shape = r,
    rate = lambda,
    leg = "Gamma"
  )

if(T) {
  # Chi-Square distribution
  d_f <- 10
  f <- function(x) { dchisq(x, d_f) }
  l_f = 0
  u_f = Inf
  init_val <- c(3, 5, 10, 15, 20, 25)
  test_Ch <- test(B, f, init_val, l_f, u_f, rchisq, dchisq, df = d_f, leg = "Chi-Square")
  test_Ch$Means
  test_Ch$`p-value`
}
if(T) {
  # Exponential distribution
  lambda <- .1
  f <- function(x) { dexp(x, lambda)}
  l_f = 0
  u_f = Inf
  init_val <- c(1.5, 2, 2.5, 3, 10, 20, 40)
  test_E <- test(B, f, init_val, l_f, u_f, rexp, dexp, rate = lambda, leg = "Exponential")
  test_E$Means
  test_E$`p-value`
  #x's must be fairly close to the mean of this distribution
  #to get reasonable results. 
}
if(T) {
  # t distribution
  d_f <- 20
  f <- function(x) { dt(x, d_f)}
  l_f = -Inf
  u_f = Inf
  init_val <- c(-10, -5, 0.75, 1, 1.5, 3, 5, 7, 10)
  test_T <- test(B, f, init_val, l_f, u_f, rt, dt, df = d_f, leg = "Student t")
  test_T$Means
  test_T$`p-value`
  #The degrees of freedom must be larger than the absolute value 
  #of the maximum and minimum of the x's.
}
if(T){
  # Gumbel distribution
  mu <- 10
  b <- 5
  f <- function(x){ dgumbel(x, mu, b)}
  l_f <- -Inf
  u_f <- Inf
  init_val <- c(0, 7, 10, 11, 12, 15, 30)
  test_G <- test(B, f, init_val, l_f, u_f, rgumbel, dgumbel, location = mu, scale = b, leg = "Gumbel")
  test_G$Means
  test_G$`p-value`
}
if(T){
  # Weibull distribution
  a <- 5
  b <- 5
  f <- function(x){ dweibull(x, a, b)}
  l_f <- 0
  u_f <- Inf
  init_val <- c(1, 3.5, 4.2, 4.5, 4.8, 6, 10)
  test_W <- test(B, f, init_val, l_f, u_f, rweibull, dweibull, shape = a, scale = b, leg = "Weibull")
  test_W$Means
  test_W$`p-value`
  #x values must be centered around mean with a somewhat wide 
  #spread to get a somewhat accurate mean.
}
if(T){
  # Pareto distribution
  a <- 1
  b <- 3
  f <- function(x){ dpareto(x, a, b)}
  l_f <- 1.0001
  u_f <- Inf
  init_val <- c(1.1, 1.2, 1.3, 1.5, 1.9, 2.5)
  test_P <- test(B, f, init_val, l_f, u_f, rpareto, dpareto, location = a, shape = b, leg = "Pareto")
  test_P$Means
  test_P$`p-value`
}
if(T){
  # F distribution
  df1 <- 3
  df2 <- 5
  f <- function(x){ df(x, df1, df2)}
  l_f <- 0
  u_f <- 50
  init_val <- c(0.01, 0.2, 0.3, 0.5, 0.6, 5, 20, 30, 40)
  test_F <- test(B, f, init_val, l_f, u_f, rf, ddensity = df, df1 = df1, df2 = df2, leg = "F")
  test_F$Means
  test_F$`p-value`
}
if(T){
  # 1/(x^5)
  f <- function(x){ x^(-5)}
  l_f <- 0.01
  u_f <- Inf
  init_val <- c(1.5, 2, 3)
  test_poly <- ars(B, f,l_f, u_f, init_val)
  mean(test_poly)
  hist(test_poly, breaks = 75, main = "1/(x^5)")
}
if(T){
  # sin(x)
  a <- 100
  f <- function(x){ a*sin(x)}
  l_f <- pi/16
  u_f <- 15*pi/16
  init_val <- c(0.5, 0.9, 1.5, 2)
  test_sin <- ars(B, f, l_f, u_f, init_val)
  mean(test_sin)
  hist(test_sin, breaks = 10, main = "sin(x)", col = "grey95")
}
