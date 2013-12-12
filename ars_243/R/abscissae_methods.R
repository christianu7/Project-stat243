
#####     Methods for "abscissae" S3 class     #####

get_zi <- function (x, ...) UseMethod("get_zi")
get_zi.abscissae <- function(x) {
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

#check.abscissae is a method that takes as an input the abscissae at any 
#iteration and checks that certain criterion are met.
#The checks are the following:
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
#Check 9: (error) Verify z_i is not empty.
#Check 10: (error) Verify dimensions of T_k and z_i are correct. z_i must 
#          always have one more element the T_k.
check <- function (x, ...) UseMethod("check")
check.abscissae <-function(abscissae) {
  
  T_k <- abscissae$T_k
  h_T <-  abscissae$h_T
  hp_T <- abscissae$hp_T
  
  #Check 1
  if(length(T_k) < 2){stop("x must be a vector of length 2 or more")}
  
  #Check 2
  for(i in 1:(length(T_k) - 1)){
    for(j in (i + 1):length(T_k)){
      if(is.logical(all.equal(T_k[i], T_k[j]))){stop("x must be a vector of unique points.")}
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
  if(sum(T_k > UB) > 0 | sum(T_k < LB) > 0){stop("x's must be chosen between the prescribed bounds")}
  
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
    else{hp_LB <- 1}
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
  #Check 9  
  if(length(abscissae$z_i == 0)){stop("The z_i is undefined.")}
  #Check 10
  if(length(abscissae$z_i) != length(abscissae$T_k) + 1){stop("The dimension of the z_i or T_k is incorrect.")}
}

plot.abscissae <- function(abscissae, plot.h=F) {
  
  if (plot.h) {
    f <- abscissae$f
    h <- function(x){ log(f(x)) }
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  par(mar=c(2,2,2,2))
  layout(matrix(c(1,1:3),2,2,byrow=T))
  
  k <- length(abscissae$T_k)
  
  if(abscissae$z_i[1]!=-Inf) {limx1 <- abscissae$z_i[1]} else {limx1 <- abscissae$z_i[2]}
  if(abscissae$z_i[k+1]!=Inf) {limx2 <- abscissae$z_i[k+1]} else {limx2 <- abscissae$z_i[k]}
  
  curve( u(x,abscissae), limx1,limx2, col="red", main="Upper and Lower functions" )
  curve( l(x,abscissae), abscissae$T_k[1], abscissae$T_k[k], col="blue", add=T )
  if (plot.h) {
    curve( h(x), limx1, limx2, add=T, col="black" )
    legend("topright",legend=c("u(x)","h(x)","l(x)"),bty="n",col=c("red","black","blue"),bg="white",lty=1)
  } else {
    legend("topright",legend=c("u(x)","l(x)"),bty="n",col=c("red","blue"),bg="white",lty=1)
  }

  curve( s(x,abscissae), limx1, limx2, col="red", main="Sampling pdf" )
  if (plot.h) {
    curve( f(x), limx1, limx2, add=T, col="black" )
    legend("topright",legend=c("s(x)","f(x)"),bty="n",col=c("red","black"),bg="white",lty=1)
  } else {
    legend("topright",legend=c("s(x)"),bty="n",col=c("red"),bg="white",lty=1)
  }
  
  curve( S(x,abscissae), limx1, limx2, col="red", main="Sampling cdf")
  abline( v=abscissae$T_k, lty=2 )
  legend("topright",legend=c("S(x)"),bty="n",col=c("red"),bg="white",lty=1)
  
  #curve( S_inv.abscissae( x , abscissae ) , 0.01, 0.99, main="Inverse cdf", col="blue")
  
  par(def.par) # reseting plot parameters
}

add_points <- function (x, ...) UseMethod("add_points")
add_points.abscissae <- function(x,new_T_k,new_h_T,new_hp_T) {
  new_order <- order(c(x$T_k,new_T_k))
  x$T_k <- c(x$T_k,new_T_k)[new_order]
  x$h_T <- c(x$h_T,new_h_T)[new_order]
  x$hp_T <- c(x$hp_T,new_hp_T)[new_order]
  x$k <- length(x$T_k)
  x$z_i <- NULL
  return(x)
}




#####     Functions using "abscissae" S3 class     #####

l <- function( x, abscissae ) {
  #browser()
  if( class(abscissae)!="abscissae" ) { stop('"abscissae" has to be an object of that class') }
  
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  j <- bucket( x , T_k )
  l_notdef <- x<min(T_k)|x>max(T_k)
  j[l_notdef] <- 1
  l <- ( (T_k[j+1]-x)*h_T[j] + (x-T_k[j])*h_T[j+1] ) / (T_k[j+1]-T_k[j])
  l[l_notdef] <- -Inf
  return( l )
}

u <- function( x , abscissae ) {
  #browser()
  if( class(abscissae)!="abscissae" ) { stop('"abscissae" has to be an object of that class') }
  
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  hp_T <- abscissae$hp_T
  z_i <- abscissae$z_i
  
  k <- length(T_k)
  j <- bucket( x , z_i )
  j[x==z_i[length(z_i)]] <- j[x==z_i[length(z_i)]] - 1 #  we would evaluate u(z_k) in the last line segment, to be well defined
  u <- h_T[j] + (x-T_k[j])*hp_T[j]
  return( u )
}

int_s <- function( abscissae ) {
  # browser()
  if( class(abscissae)!="abscissae" ) { stop('"abscissae" has to be an object of that class') }
  
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  hp_T <- abscissae$hp_T
  z_i <- abscissae$z_i
  k <- length(T_k)
  sum(
    (1/hp_T) * ( exp( u(z_i[-1],abscissae) ) - exp( u(z_i[-(k+1)],abscissae) ) )
  )
}

s <- function( x, abscissae ) {
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  hp_T <- abscissae$hp_T
  z_i <- abscissae$z_i
  (1/int_s( abscissae )) * exp( u( x, abscissae ) )
}

S <- function( x , abscissae ) {
  # browser()
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  hp_T <- abscissae$hp_T
  z_i <- abscissae$z_i
  
  # gives the interval of each x, corresponding with the segments of lines
  k <- length(T_k)
  
  j <- bucket( x , z_i )
  j[x==z_i[length(z_i)]] <- j[x==z_i[length(z_i)]] - 1 #  we would evaluate u(z_k) in the last line segment, to be well defined
  
  #k <- length(T_k)
  # vector with integral of exp-trapeziods areas
  int_trap <- (1/hp_T) * ( exp( u(z_i[-1], abscissae) ) - exp( u(z_i[-(k+1)], abscissae) ) )
  
  # for each x, calculate the area of the trapezoids before z_j-i
  int_trap_mat <- matrix(int_trap,length(x),length(int_trap),byrow=T)
  j_mat <- matrix(j,length(x),length(int_trap))
  int_mat <- matrix(1:length(int_trap),length(x),length(int_trap),byrow=T)
  int_trap_mat[ int_mat >= j_mat ] <- 0
  int_bef <- apply( int_trap_mat ,1,sum)
  
  cdf <- (1/int_s( abscissae )) * ( int_bef + (1/hp_T[j]) * ( exp( u(x, abscissae) ) - exp( u(z_i[j], abscissae) ) ) )
  return(cdf)
}

S_inv <- function( x , abscissae ) {
  #browser()
  if( class(abscissae)!="abscissae" ) { stop('"abscissae" has to be an object of that class') }
  
  if( any(x<0|x>1) ) { stop("Some values of x are <0 or >1") }
  
  T_k <- abscissae$T_k
  h_T <- abscissae$h_T
  hp_T <- abscissae$hp_T
  z_i <- abscissae$z_i
  
  k <- length(T_k)
  
  j <- bucket( x,
               S( x=z_i, abscissae )
  )
  j[x==0] <- 1
  j[x==1] <- length(T_k)
  
  # vector with integral of exp-trapeziods areas
  int_trap <- (1/hp_T) * ( exp( u( x=z_i[-1], abscissae ) ) - exp( u( x=z_i[-(k+1)], abscissae) ) )
  
  # for each x, calculate the area of the trapezoids before z_j-i
  int_trap_mat <- matrix(int_trap,length(x),length(int_trap),byrow=T)
  j_mat <- matrix(j,length(x),length(int_trap))
  int_mat <- matrix(1:length(int_trap),length(x),length(int_trap),byrow=T)
  int_trap_mat[ int_mat >= j_mat ] <- 0
  int_bef <- apply( int_trap_mat ,1,sum)
  
  cdf_inf <- ( log( hp_T[j] * ( x * int_s( abscissae ) - int_bef ) + exp( u(x=z_i[j], abscissae) ) )  - h_T[j] )/hp_T[j] + T_k[j]
  return(cdf_inf)
}

#The following function performs a non-parametric test (permutation test)
#which tests for a difference in distributions. We will use it to test
#for difference between the ARS sample distribution and the theoretical
#distribution. The p-value returned is small if the distributions are 
#not the same.
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

#The following function samples from the desired distribution using 
#the ARS algorithm and then samples from the distribution using random 
#number generator functions in R. It performs a permutation test on
#the two samples to check for the similarity between distributions.
#It returns the two samples, the means of each sample and the p-value
#from the permutation test.
test <- function(B, f, init_val, l_f, u_f, rdensity, ddensity, ep = 1e-3, leg="Density", ...){ 
  
  samp_ars <- ars(B, f, init_val, l_f, u_f, ep = 1e-5)
  samp_theo <- rdensity(B,...)
  m_ars <- mean(samp_ars)
  m_theo <- mean(samp_theo)
  mean <- c(m_ars, m_theo)
  names(mean) <- c("ARS Sample", "Theoretical Sample")
  p <- permdiff(samp_ars, samp_theo, 2000)
  ARS <- list(samp_ars, samp_theo, mean, p)
  names(ARS) <- c("ARS Sample", "Theoretical Sample", "Means", "p-value")
  hist(samp_ars, freq = F, col = "grey95", main = "Density", xlab = "ARS Sample")
  legend("topright",legend=leg,bty="n")
  density <- function(x){ ddensity(x, ...)}
  curve(density, add = T, col = "blue" )
  return(ARS)
}