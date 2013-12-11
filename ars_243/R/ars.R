
#####     Adaptative rejecting sampling method     #####

ars <- function( B=100, f ,l_f=-Inf, u_f=Inf, init_abs=NULL, ep=1e-10 , m=10, evol.gif=F ) {
  # require(ars_243)
  # browser()
  if ( is.expression(f) ) {
    f_exp <- f
    f <- function(x) { eval(f_exp) }
  }
  if (!is.function(f)) {
    stop( '"f" has to be either expr or function' )
  }

  h <- function(x){ log(f(x)) }

  # Initial Points
  if (is.null(init_abs)) {
    abscissae <- init_abs( h, l_h=l_f, u_h=u_f)
  } else {
    abscissae <- as.abscissae( init_abs, h, l_h=l_f, u_h=u_f, eps=eps )
  }
  check(abscissae)
  abscissae <- get_zi( abscissae )
    
  #####     Simulation     #####
  
  sim_values <- as.numeric(NULL)
  # Numbers of points tested on each iteration
  iter <- 0
  
  if( evol.gif ) {
    def.par <- par(no.readonly = TRUE) # save default plot settings, for resetting...
  }
  
  while (length(sim_values)<B) {
  #while (iter<10) {
    iter <- iter + 1
    # sampling uniform for rejection sampling
    w <- runif(m,0,1)
    
    # sampling from s(x)
    x_star <- S_inv( runif(m,0,1) , abscissae )
    
    # Plot of accepted and rejected points in phase 1 or 2
    if( evol.gif ) {
      layout(matrix(1:2))
      par(mar=c(0,0,0,0)+2)
      
      curve(f(x),l_f,u_f,main="f(x) and s(x)")
      curve(s(x,abscissae),l_f,u_f,col="blue",add=T)
      
      curve( exp( h(x) - u(x,abscissae) ) ,
            l_f, u_f, col="darkgreen", lty=1, main="Squeezing and Rejection areas", ylim=c(0,1), ylab="" )
      curve( exp( l(x,abscissae) - u(x,abscissae)),
            min(abscissae$T_k), max(abscissae$T_k), col="darkgreen", lty=2, add=T )
      abline(v=abscissae$z_i,col="orange")
      points(x=x_star,y=w,col="red",pch=19)
      #points(x_star[accept_1],w[accept_1],col="green",pch=19)
      #points(x_star[!accept_1][accept_2],w[!accept_1][accept_2],col="gold",pch=19)
    }
    
    ### Testing sample ###
    # (1) squeezing test
      accept_1 <- ( w <= exp( l(x_star,abscissae) - u(x_star,abscissae) ) )
      accept_1[is.na(accept_1)] <- F # those sampled values not defined in l(x)
      if (any(accept_1)) {
        sim_values <- c(sim_values,x_star[accept_1])
      }
    # rejection test
    if( any(!accept_1) ) {
      new_T_k <- x_star[!accept_1]
      new_h_T <-  h(new_T_k)
      new_hp_T <- ( h(new_T_k+eps) - new_h_T ) / eps
      
      accept_2 <- ( w[!accept_1] <= exp( new_h_T - u(new_T_k,abscissae) ) )
      if (any(accept_2)) {
        sim_values <- c(sim_values,new_T_k[accept_2])
      }

      # Add to abscissae those point evaluated in h(x)
      abscissae <- add_points.abscissae( abscissae, new_T_k, new_h_T, new_hp_T )
      abscissae <- get_zi( abscissae )
    }
    
  }

  if( evol.gif ) {
    par(def.par)  #- reset plot settings to default
  }
  
  return( sim_values )
  
}
