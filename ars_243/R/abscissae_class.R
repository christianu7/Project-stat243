
####   S3 class definition   ###

# Constructor #

# Creates the objects that contains the necesary information to get our sample
as.abscissae <- function( x, f, l_h, u_h, eps ) {
  if ( is.expression(f) ) {
    f <- eval(parse(text=paste( "function(x) {",as.character(f),"}" )))
  }
  if (!is.function(f)) {
    stop( '"f" has to be either expression or function in terms of "x"' )
  }
  h <- function(x){ log(f(x)) }
  
  T_k <- x
  h_T <-  h(T_k)
  hp_T <- ( h(T_k+eps) - h_T ) / eps
  
  k <- length(T_k)
  abscissae <- structure(
    list(
          f=f,
          T_k=T_k,
          h_T=h_T,
          hp_T=hp_T,
          z_i=NULL,
          k=k,
          l_h=l_h,
          u_h=u_h )
    , class = "abscissae"
  )
  return(abscissae)
}
