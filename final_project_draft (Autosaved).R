##########FINAL PROJECT DRAFT############
rm(list=ls())

#install.packages('VGAM')
#require('VGAM')

B <- 1000
ep=1e-07

#For a lot of the following distributions, epsilon must be fairly 
#large like 1e-2 or 1e-3, in order to get reasonable results.

#par(mfrow = c(3, 4))
par(mfrow = c(1, 1))

if(T) {
  # Normal distribution
  mu <- 5
  sigma <- 2
  #f <- expression( (2*pi*sigma^2)^(-1/2) * exp(-(x-mu)^2/(2*sigma^2)) )
  f <- function(x) {dnorm(x,mu,sigma)}
  l_f = -Inf #mu - 10*sigma
  u_f = Inf #mu + 10*sigma
  init_val <- c(-10, 3, 4, 5, 6, 15)
  test_N <- test(10000, f, init_val, l_f, u_f, rnorm, mean = mu, sd = sigma)
  test_N$Means
  test_N$`p-value`
}
if(T) {
  # Beta distribution
  a <- 5
  b <- 2
  f <- function(x) {dbeta(x,a,b)}
  l_f = 0
  u_f = 1
  init_val <- c(.4, .6, .9)
  test_B <- test(10000, f, init_val, l_f, u_f, rbeta, shape1 = a, shape2 = b)
  test_B$Means
  test_B$`p-value`
}
if(T) {
  # Gamma distribution
  r=15
  lambda=1
  f <- function(x) {dgamma(x,r,lambda)}
  l_f = 0
  u_f = Inf
  init_val <- c(5, 10, 15, 20, 25, 40)
  test_Gm <- test(10000, f, init_val, l_f, u_f, rgamma, shape = r, rate = lambda)
  test_Gm$Means
  test_Gm$`p-value`
}
if(T) {
  # Chi-Square distribution
  df <- 10
  f <- function(x) { dchisq(x, df) }
  l_f = 0
  u_f = Inf
  init_val <- c(3, 5, 10, 15, 20, 25)
  test_Ch <- test(10000, f, init_val, l_f, u_f, rchisq, df = df)
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
  test_E <- test(10000, f, init_val, l_f, u_f, rexp, rate = lambda)
  test_E$Means
  test_E$`p-value`
  #x's must be fairly close to the mean of this distribution
  #to get reasonable results. 
}
if(T) {
  # t distribution
  df<- 20
  f <- function(x) { dt(x, df)}
  l_f = -Inf
  u_f = Inf
  init_val <- c(-10, -5, 0.75, 1, 1.5, 3, 5, 7, 10)
  test_T <- test(10000, f, init_val, l_f, u_f, rt, df = df)
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
  test_G <- test(10000, f, init_val, l_f, u_f, rgumbel, location = mu, scale = b)
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
  test_W <- test(10000, f, init_val, l_f, u_f, rweibull, shape = a, scale = b)
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
  test_P <- test(10000, f, init_val, l_f, u_f, rpareto, location = a, shape = b)
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
  test_F <- test(10000, f, init_val, l_f, u_f, rf, df1 = df1, df2 = df2)
  test_F$Means
  test_F$`p-value`
}
if(T){
  # 1/(x^5)
  f <- function(x){ x^(-5)}
  l_f <- 0.01
  u_f <- Inf
  init_val <- c(1.5, 2, 3)
  test_poly <- ars(10000, f, init_val, l_f, u_f, ep = 1e-3)
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
  test_sin <- ars(10000, f, init_val, l_f, u_f, ep = 1e-3)
  mean(test_sin)
  hist(test_sin, breaks = 100, main = "sin(x)")
}

permdiff = function(x,y,B){
  k = mean(x) - mean(y)
  P_Stat = numeric(B)
  for (b in 1:B){
    m = length(x)
    z = c(x, y)
    Ind = sample(length(z))
    P_Stat[b] = mean(z[Ind[1:m]])-mean(z[Ind[-(1:m)]])
  }
  R = sum(P_Stat >= k)
  p = R/(B + 1)
  return(p)
}


test <- function(B, f, init_val, l_f, u_f, rdensity, ...){ 
  
  samp_ars <- ars(B, f, init_val, l_f, u_f, ep = 1e-5)
  samp_theo <- rdensity(B,...)
  m_ars <- mean(samp_ars)
  m_theo <- mean(samp_theo)
  mean <- c(m_ars, m_theo)
  names(mean) <- c("ARS Sample", "Theoretical Sample")
  p <- permdiff(samp_ars, samp_theo, 2000)
  ARS <- list(samp_ars, samp_theo, mean, p)
  names(ARS) <- c("ARS Sample", "Theoretical Sample", "Means", "p-value")
  #par(mfrow = c(1, 2))
  hist(samp_ars, breaks = B/10, main = "ARS Sample")
  #hist(samp_theo, breaks = B/10, main = "Theoretical Sample")
  return(ARS)
}

ars <- function( B = 100, f , init_val, l_f = -Inf, u_f = Inf, ep=1e-3) {
  
  if ( is.expression(f) ) {
    f_exp <- f
    f <- function(x) { eval(f_exp) }
  }
  if (!is.function(f)) {
    stop( '"f" has to be either expr of function' )
  }
  
  h <- function(x){ log(f(x)) }
  
  ######START INITIAL VALUE FUNCTION#########
  
  #initVal is a function that takes as inputs the following:
  #h is the log of a density.
  #bound gives the support of the density.
  #x is a vector of points to create the initial abscissae of length greater than 2.
  #ep is used in numerical differentiation of h.
  #initVal returns the initial abscissae, h and h prime at the abscissae, 
  #the upper and lower bounds of the domain, the length of the initial abscissae, 
  #and a place holder for the zi's.
  
  #Several checks are in this function to make sure the initial points are valid:
  #Check 1: (error) x must have length greater than or equal to 2.
  #Check 2: (error) x must contain unique elements.
  #Check 3: (warning) The upper bound must be greater than the lower bound.
  #Check 4: (error) Values of x must be within the upper and lower bounds.
  #Check 5: (error) Values of x must have mass in f, or h will not be finite.
  #Check 6: (warning) Some values of x won't be used if h at that x is infinite,
  #         but Check 1 must still hold.
  #Check 7: (error) Where there is a global max on the domain of h, 
  #         there must be at least one point on either side of the max.
  #Check 8: (error) A naive check for concavity of h by checking derivates at bounds,
  #         where bounds are finite.
  
  as.abscissae <- function(init_val, bound = c(l_f, u_f), ep = ep){ 
    
    #Check 1
    if(length(init_val) < 2){stop("x must be a vector of length 2 or more")}
    
    #Check 2
    for(i in 1:(length(init_val) - 1)){
      for(j in (i + 1):length(init_val)){
        if(is.logical(all.equal(init_val[i], init_val[j]))){stop("x must be a vector of unique points.")}
      }
    }
    
    #Check 3
    if(bound[1] > bound[2]){
      warning("Upper bound must be greater than Lower bound")
      LB <- bound[order(bound)[1]]
      UB <- bound[order(bound)[2]]
    }
    else{LB <- bound[1]; UB <- bound[2]}
    
    #Check 4
    if(sum(init_val > UB) > 0 | sum(init_val < LB) > 0){stop("x's must be chosen between the prescribed bounds")}
    
    
    T_k <- init_val
    h_T <-  h(T_k)
    hp_T <- ( h(T_k+ep) - h_T ) / ep
    
    T_k <- T_k[which(is.infinite(h_T) == F)]
    h_T <- h_T[which(is.infinite(h_T) == F)]
    hp_T <- hp_T[which(is.infinite(h_T) == F)]
    
    #Check 5
    if(length(T_k) < 2){stop("Please input different x such that f has more mass at x.")}
    
    #Check 6
    if(length(T_k) != length(init_val)){warning("One or more of the intitial points weren't used.")}
    
    k <- length(T_k)
    
    #Check 7
    if(is.infinite(LB) && is.infinite(UB)){
      i <- 1
      while(prod(hp_T[i], hp_T[i + 1]) > 0 && i < length(T_k)){
        i <- i + 1
      }
      if(i == length(T_k)){stop("Choose x's such that they straddle the mode of f or choose more appropriate bounds.")}
    }
    if(!is.infinite(LB) && is.infinite(UB)){
      h_LB <- h(LB)
      if(!is.infinite(h_LB)){
        hp_LB <- ( h(LB + ep) - h_LB ) / ep 
      }
      else{hp_LB <- 1}
      if(hp_LB > 0){
        i <- 1
        while(prod(hp_T[i], hp_T[i + 1]) > 0 && i < length(T_k)){
          i <- i + 1
        }
        if(i == length(T_k)){stop("Choose x's such that they straddle the mode of f or choose more appropriate bounds.")}
      }
    }
    if(is.infinite(LB) && !is.infinite(UB)){
      h_UB <- h(UB)
      if(!is.infinite(h_UB)){
        hp_UB <- ( h(UB + ep) - h_UB ) / ep 
      }
      else{hp_UB <- -1}
      if(hp_UB < 0){
        i <- 1
        while(prod(hp_T[i], hp_T[i + 1]) > 0 && i < length(T_k)){
          i <- i + 1
        }
        if(i == length(T_k)){stop("Choose x's such that they straddle the mode of f or choose more appropriate bounds.")}
      }
    }
    if(!is.infinite(LB) && !is.infinite(UB)){
      h_LB <- h(LB)
      h_UB <- h(UB)
      if(!is.infinite(h_LB)){
        hp_LB <- ( h(LB + ep) - h_LB ) / ep 
      }
      {hp_LB <- 1}
      if(!is.infinite(h_UB)){
        hp_UB <- ( h(UB + ep) - h_UB ) / ep 
      }
      else{hp_UB <- -1}
      if(hp_LB > 0 && hp_UB < 0){
        i <- 1
        while(prod(hp_T[i], hp_T[i + 1]) > 0 && i < length(T_k)){
          i <- i + 1
        }
        if(i == length(T_k)){stop("Choose x's such that they straddle the mode of f or choose more appropriate bounds.")}
      }
      
      #Check 8
      if(hp_LB < 0 && hp_UB > 0){stop("f is not log concave!")}
    }
    
    A <- list(T_k = T_k, h_T = h_T, hp_T = hp_T, z_i=NULL, k = k, LB = LB, UB = UB)
    names(A) <- c("T_k", "h_T", "hp_T", "z_i", "k", "l_h", "u_h")
    return(A)  
  }
  
  get_z_i <- function(x) {
    #browser()
    T_k <- x$T_k
    h_T <-  x$h_T
    hp_T <- x$hp_T
    k <- length(T_k)
    z_i <- ( h_T[-1] - h_T[-k] - T_k[-1] * hp_T[-1] + T_k[-k] * hp_T[-k] ) / (hp_T[-k]-hp_T[-1])
    if( any(is.na(z_i)|is.nan(z_i)) ) { stop("Some elements of z_i could not be calculated") }
    z_i <- c(x$l_h,z_i,x$u_h)
    x$z_i <- z_i
    return(x)
  }
  
  # Initial Points
  abscissae <- as.abscissae(init_val = init_val, c(l_f, u_f), ep = ep)
  abscissae <- get_z_i( abscissae )
  
  bucket <- function( x, x_i ) {
    # this function returns the interval in which x is located corresponding with the values of x_i
    # 0 if x < x_i[1]
    # 1 if x >=  x_i[1]  and x < x_i[2]
    # ...
    # j if x >=  x_i[j]  and x < x_i[j+1]
    # ...
    # k if x >= x_i[k]
    
    x_i_mat <- matrix(x_i,length(x),length(x_i),byrow=T)
    c1 <- cbind( T, x >= x_i_mat )
    c2 <- cbind( x < x_i_mat , T )
    b <- rep(0:length(x_i),length(x))[as.vector(t(c1 & c2))]
    return( b )
  }
  
  l <- function( x , T_k , h_T ) {
    #browser()
    j <- bucket( x , T_k )
    l_notdef <- x<min(T_k)|x>max(T_k)
    j[l_notdef] <- 1
    l <- ( (T_k[j+1]-x)*h_T[j] + (x-T_k[j])*h_T[j+1] ) / (T_k[j+1]-T_k[j])
    l[l_notdef] <- 0
    return( l )
  }
  
  u <- function( x , T_k , h_T , hp_T , z_i ) {
    #browser()
    k <- length(T_k)
    j <- bucket( x , z_i )
    j[x==z_i[length(z_i)]] <- j[x==z_i[length(z_i)]] - 1 #  we would evaluate u(z_k) in the last line segment, to be well defined
    u <- h_T[j] + (x-T_k[j])*hp_T[j]
    return( u )
  }
  #curve(h(x),l_f,u_f)
  #curve(l(x,T_k=abscissae$T_k,h_T=abscissae$h_T),min(abscissae$T_k),max(abscissae$T_k),add=T,col="red")
  #curve(u(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i),l_f+ep,u_f-ep,add=T,col="blue")
  
  int_s <- function( T_k, h_T, hp_T, z_i ) {
    # browser()
    k <- length(T_k)
    sum(
      (1/hp_T) * ( exp( u(z_i[-1], T_k , h_T , hp_T , z_i) ) - exp( u(z_i[-(k+1)], T_k , h_T , hp_T , z_i) ) )
    )
  }
  int_s( T_k=abscissae$T_k , h_T=abscissae$h_T , hp_T=abscissae$hp_T , z_i=abscissae$z_i )
  
  s <- function( x , T_k, h_T , hp_T , z_i ) {
    (1/int_s( T_k, h_T, hp_T, z_i )) * exp( u( x, T_k, h_T, hp_T, z_i ) )
    
  }
  #curve(f(x),l_f,u_f)
  #curve(s(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i),l_f,u_f,col="blue",add=T)
  
  S <- function( x , T_k, h_T , hp_T , z_i ) {
    # browser()
    # gives the interval of each x, corresponding with the segments of lines
    k <- length(T_k)
    
    j <- bucket( x , z_i )
    j[x==z_i[length(z_i)]] <- j[x==z_i[length(z_i)]] - 1 #  we would evaluate u(z_k) in the last line segment, to be well defined
    
    #k <- length(T_k)
    # vector with integral of exp-trapeziods areas
    int_trap <- (1/hp_T) * ( exp( u(z_i[-1], T_k , h_T , hp_T , z_i) ) - exp( u(z_i[-(k+1)], T_k , h_T , hp_T , z_i) ) )
    
    # for each x, calculate the area of the trapezoids before z_j-i
    int_trap_mat <- matrix(int_trap,length(x),length(int_trap),byrow=T)
    j_mat <- matrix(j,length(x),length(int_trap))
    int_mat <- matrix(1:length(int_trap),length(x),length(int_trap),byrow=T)
    int_trap_mat[ int_mat >= j_mat ] <- 0
    int_bef <- apply( int_trap_mat ,1,sum)
    
    cdf <- (1/int_s( T_k, h_T, hp_T, z_i )) * ( int_bef + (1/hp_T[j]) * ( exp( u(x, T_k , h_T , hp_T , z_i) ) - exp( u(z_i[j], T_k , h_T , hp_T , z_i) ) ) )
    return(cdf)
  }
  
  #curve(S(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i),l_f,u_f,col="blue")
  #abline(v=abscissae$T_k,lty=2)
  
  S_inv <- function( x , T_k, h_T , hp_T , z_i ) {
    #browser()
    
    if( any(x<0|x>1) ) { stop("Some values of x are <0 or >1") }
    
    k <- length(T_k)
    
    j <- bucket( x,
                 S(x=z_i,T_k,h_T,hp_T,z_i)
    )
    j[x==0] <- 1
    j[x==1] <- length(T_k)
    
    # vector with integral of exp-trapeziods areas
    int_trap <- (1/hp_T) * ( exp( u(z_i[-1], T_k , h_T , hp_T , z_i) ) - exp( u(z_i[-(k+1)], T_k , h_T , hp_T , z_i) ) )
    
    # for each x, calculate the area of the trapezoids before z_j-i
    int_trap_mat <- matrix(int_trap,length(x),length(int_trap),byrow=T)
    j_mat <- matrix(j,length(x),length(int_trap))
    int_mat <- matrix(1:length(int_trap),length(x),length(int_trap),byrow=T)
    int_trap_mat[ int_mat >= j_mat ] <- 0
    int_bef <- apply( int_trap_mat ,1,sum)
    
    cdf_inf <- ( log( hp_T[j] * ( x * int_s( T_k, h_T, hp_T, z_i ) - int_bef ) + exp( u(z_i[j], T_k , h_T , hp_T , z_i) ) )  - h_T[j] )/hp_T[j] + T_k[j]
    return(cdf_inf)
  }
  #curve( S_inv( x , T_k=abscissae$T_k, h_T=abscissae$h_T, hp_T=abscissae$hp_T, z_i=abscissae$z_i ) , 0.01, 0.99, col="blue")
  
  #####     Simulation     #####
  
  sim_values <- as.numeric(NULL)
  # Numbers of points tested on each iteration
  #set.seed(1)
  n <- 2
  iter <- 0
  while (length(sim_values)<B) {
    m <- 2^n
    #m <- 10
    #while (iter<10) {
    iter <- iter + 1
    # sampling uniform for rejection sampling
    w <- runif(m,0,1)
    
    # sampling from s(x)
    x_star <- S_inv( runif(m,0,1) , T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i )
    
    # Plot of accepted and rejected points in phase 1 or 2
    if( F ) {
      layout(matrix(1:2))
      par(mar=c(0,0,0,0)+2)
      
      curve(f(x),l_f,u_f,main="f(x) and s(x)")
      curve(s(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i),l_f,u_f,col="blue",add=T)
      
      curve(exp(h(x) -
                  u(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i)),l_f,u_f,col="darkgreen",lty=1, main="Squeezing and Rejection areas",ylim=c(0,1),ylab="")
      curve(exp(l(x,T_k=abscissae$T_k,h_T=abscissae$h_T) -
                  u(x,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i)),min(abscissae$T_k),max(abscissae$T_k),col="darkgreen",lty=2,add=T)
      abline(v=abscissae$z_i,col="orange")
      points(x=x_star,y=w,col="red",pch=19)
      #points(x_star[accept_1],w[accept_1],col="green",pch=19)
      #points(x_star[!accept_1][accept_2],w[!accept_1][accept_2],col="gold",pch=19)
    }
    
    ### Testing ###
    # (1) squeezing test
    accept_1 <- ( w <= exp(l(x_star,T_k=abscissae$T_k,h_T=abscissae$h_T) -
                             u(x_star,T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i)) )
    accept_1[is.na(accept_1)] <- F # those sampled values not defined in l(x)
    if (any(accept_1)) {
      sim_values <- c(sim_values,x_star[accept_1])
    }
    # rejection test
    if( any(!accept_1) ) {
      new_T_k <- x_star[!accept_1]
      new_h_T <-  h(new_T_k)
      new_hp_T <- ( h(new_T_k+ep) - new_h_T ) / ep
      
      accept_2 <- ( w[!accept_1] <= exp(new_h_T - u( new_T_k , T_k=abscissae$T_k,h_T=abscissae$h_T,hp_T=abscissae$hp_T,z_i=abscissae$z_i ) ) )
      if (any(accept_2)) {
        sim_values <- c(sim_values,new_T_k[accept_2])
      }
      
      # Add to abscissae those point evaluated in h(x)
      new_order <- order(c(abscissae$T_k,new_T_k))
      abscissae$T_k <- c(abscissae$T_k,new_T_k)[new_order]
      abscissae$h_T <- c(abscissae$h_T,new_h_T)[new_order]
      abscissae$hp_T <- c(abscissae$hp_T,new_hp_T)[new_order]
      abscissae$k <- length(abscissae$Tk)
      abscissae$z_i <- NULL
      
      abscissae <- get_z_i( abscissae )
    }
    n <- n + 1
    
  }
  return(sim_values)
  
}

