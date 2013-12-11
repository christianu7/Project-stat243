######START INITIAL VALUE FUNCTION#########


#initVal is a function that takes as inputs the following:
#h is the log of a density.
#bound gives the support of the density.
#x is a vector of points to create the initial abscissae of length greater than 2.
#eps is used in numerical differentiation of h.
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


initVal <- function(h, bound = c(-Inf, Inf), x, eps = 1e-10){ 
  
  #Check 1
  if(length(x) < 2){stop("x must be a vector of length 2 or more")}
  
  #Check 2
  for(i in 1:(length(x) - 1)){
    for(j in (i + 1):length(x)){
      if(is.logical(all.equal(x[i], x[j]))){stop("x must be a vector of unique points.")}
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
  if(sum(x > UB) > 0 | sum(x < LB) > 0){stop("x's must be chosen between the prescribed bounds")}
  
  
  T_k <- x
  h_T <-  h(T_k)
  hp_T <- ( h(T_k+eps) - h_T ) / eps
  
  T_k <- T_k[which(is.infinite(h_T) == F)]
  h_T <- h_T[which(is.infinite(h_T) == F)]
  hp_T <- hp_T[which(is.infinite(h_T) == F)]
  
  #Check 5
  if(length(T_k) < 2){stop("Please input different x such that f has more mass at x.")}
  
  #Check 6
  if(length(T_k) != length(x)){warning("One or more of the intitial points weren't used.")}
  
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
      hp_LB <- ( h(LB + eps) - h_LB ) / eps 
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
      hp_UB <- ( h(UB + eps) - h_UB ) / eps 
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
      hp_LB <- ( h(LB + eps) - h_LB ) / eps 
    }
    else{hp_LB <- 1}
    if(!is.infinite(h_UB)){
      hp_UB <- ( h(UB + eps) - h_UB ) / eps 
    }
    else{hp_UB <- -1}
    if(hp_LB > 0 && hp_UB < 0){
      i <- 1
      while(prod(hp_T[i], hp_T[i + 1]) > 0 && i < length(T_k)){
        i <- i + 1
      }
      if(i == length(T_k)){stop("Choose x's such that they straddle the mode of f or choose more appropriate bounds.")}
    }
    if(hp_LB < 0 && hp_UB > 0){stop("f is not log concave!")}
  }
  
  A <- list(T_k = T_k, h_T = h_T, hp_T = hp_T, z_i=NULL, k = k, LB = LB, UB = UB)
  return(A)
  
}


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

if(F){x <- c(-100, 0, 100)}
if(F){x <- c(0, 0, 100)}
if(F){x <- c(0, 1, 5, 15, 100)}
if(F){x <- c(1)}
if(F){x <- c(10, -100, 1000)}
if(F){x <- c(-1000, -100, 1000)}
if(F){x <- c( 1, 5, 10, 11, 12, 13, 15, 35)}
if(F){x <- c(1, 2, 3, 4, 5)}

initVal(h, bound = bound, x)