
#####     Methods for "abscissae" S3 class     #####

get_zi <- function (x, ...) UseMethod("get_zi")
get_zi.abscissae <- function(x) {
  #UseMethod("abscissae")
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

check <- function (x, ...) UseMethod("check")
check.abscissae <-function(abscissae) {
  pass <- T
  if(!pass) { stop("The abscissae does not meet criterion x") }
}

plot.abscissae <- function(abscissae, plot.h=F) {
  
  if (plot.h) {
    h <- function(x) { eval(abscissae$h) }
    f <- function(x){ exp(h(x)) }
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  par(mar=c(2,2,2,2))
  layout(matrix(c(1,1:3),2,2,byrow=T))
  
  curve( l(x,abscissae), min(abscissae$T_k), max(abscissae$T_k), col="red", main="Upper and Lower functions" )
  curve( u(x,abscissae), l_f+eps, u_f-eps, add=T, col="blue" )
  if (plot.h) { curve( h(x), l_f, u_f, add=T) }
  
  curve( s(x,abscissae), l_f, u_f, col="blue", main="Sampling pdf" )
  if (plot.h) { curve( f(x), l_f, u_f, add=T ) }
  
  curve( S(x,abscissae), l_f, u_f, col="blue", main="Sampling cdf")
  abline( v=abscissae$T_k, lty=2 )
  
  #curve( S_inv.abscissae( x , abscissae ) , 0.01, 0.99, main="Inverse cdf", col="blue")
  
  par(def.par) # reseting plot parameters
}

add_points <- function (x, ...) UseMethod("add_points")
add_points.abscissae <- function(x,new_T_k,new_h_T,new_hp_T) {
  new_order <- order(c(x$T_k,new_T_k))
  x$T_k <- c(x$T_k,new_T_k)[new_order]
  x$h_T <- c(x$h_T,new_h_T)[new_order]
  x$hp_T <- c(x$hp_T,new_hp_T)[new_order]
  x$k <- length(x$Tk)
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
  l[l_notdef] <- 0
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
  par(mfrow = c(1, 2))
  hist(samp_ars, breaks = B/10, main = "ARS Sample")
  hist(samp_theo, breaks = B/10, main = "Theoretical Sample")
  return(ARS)
}