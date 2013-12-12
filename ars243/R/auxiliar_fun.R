#####     Auxiliar Generic Functions     #####

deriv_num <- function(fun,x,h=1e-5) {
  # function for two-sided numerical derivation
    ( fun(x+h)-fun(x-h) ) / (2*h)
  }

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
