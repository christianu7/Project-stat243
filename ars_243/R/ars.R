
#####     Adaptative rejecting sampling method     #####

ars <- function( B=100, f ,l_f=-Inf, u_f=Inf, init_abs=NULL, eps=1e-10 , m="exp", rej_evol.pdf=NULL, abs_evol.pdf=NULL, hist.pdf=NULL ) {
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
    abscissae <- as.abscissae( init_abs, f, l_h=l_f, u_h=u_f, eps=eps )
  }
  #check(abscissae)
  abscissae <- get_zi( abscissae )
  #check(abscissae)
  
  #####     Simulation     #####
  
  sim_values <- as.numeric(NULL)
  # Numbers of points tested on each iteration
  iter <- 0
  
  def.par <- par(no.readonly = TRUE) # save default plot settings, for resetting...
  dev_orig <- dev.list()
    
  if( !is.null(rej_evol.pdf) ) {
    pdf(file=rej_evol.pdf,width=10, height=7, onefile=T)
    dev_rej <- dev.cur()
  }
  
  if( !is.null(abs_evol.pdf) ) {
    pdf(file=abs_evol.pdf,width=10, height=7, onefile=T)
    dev_abs <- dev.cur()
  }
  
  m_orig <- m
  while (length(sim_values)<B) {
  #while (iter<10) {
    iter <- iter + 1
    
    if(m_orig=="exp") { m<-2^iter }
    if(m_orig=="lin") { m<-2*iter }
    suppressWarnings( m <- as.numeric(m) )
    if( is.na(m) ) {stop('"m" has to be either a number or any of "exp", or "lin"')}
    
    if( !is.null(abs_evol.pdf) ) {
      dev.set(dev_abs)
      plot( abscissae, plot.h=T )
    }
    
    # sampling uniform for rejection sampling
    w <- runif(m,0,1)
    
    # sampling from s(x)
    x_star <- S_inv( runif(m,0,1) , abscissae )
    
    ### Testing sample ###
    # (1) squeezing test
      accept_1 <- ( w <= exp( l(x_star,abscissae) - u(x_star,abscissae) ) )
      accept_1[is.na(accept_1)] <- F # those sampled values not defined in l(x)
      if (any(accept_1)) {
        sim_values <- c(sim_values,x_star[accept_1])
      }
    
    # limits for graphics
    k <- length(abscissae$T_k)
    if(abscissae$z_i[1]!=-Inf) {limx1 <- abscissae$z_i[1]} else {limx1 <- abscissae$z_i[2]}
    if(abscissae$z_i[k+1]!=Inf) {limx2 <- abscissae$z_i[k+1]} else {limx2 <- abscissae$z_i[k]}
    
    # Plot accepted points in phase 1
    if( !is.null(rej_evol.pdf) ) {
      dev.set(dev_rej)
      layout(matrix(1:2))
      par(mar=c(0,0,0,0)+2)
      
      curve(f(x),limx1,limx2,main="f(x) and s(x)",col="black")
      curve(s(x,abscissae),limx1,limx2,col="red",add=T)
      legend("topright",legend=c("s(x)","f(x)"),col=c("red","black"),bg="white",lty=1)
      
      curve( exp( h(x) - u(x,abscissae) ) ,
             limx1, limx2, col="darkgreen", lty=1, main="Squeezing and Rejection areas", ylim=c(0,1), ylab="" )
      curve( exp( l(x,abscissae) - u(x,abscissae)),
             min(abscissae$T_k), max(abscissae$T_k), col="darkgreen", lty=2, add=T )
      abline(v=abscissae$z_i,col="orange",lty=3)
      points(x=x_star[accept_1],w[accept_1],col="green",pch=19)
      legend("topleft",legend=c("phase 1","phase 2","rejected"),col=c("green","gold","red"),bg="white",pch=19)
      legend("topright",legend=c("exp(h(x)-u(x))","exp(l(x)-u(x))","z_i"),col=c("darkgreen","darkgreen","orange"),bg="white",lty=c(1,2,3))
    }

    ### Testing sample ###
    # (2) rejection test
    
    if( any(!accept_1) ) {
      new_T_k <- x_star[!accept_1]
      new_h_T <-  h(new_T_k)
      new_hp_T <- ( h(new_T_k+eps) - new_h_T ) / eps
      
      accept_2 <- ( w[!accept_1] <= exp( new_h_T - u(new_T_k,abscissae) ) )
      if (any(accept_2)) {
        sim_values <- c(sim_values,new_T_k[accept_2])
      }

      # Plot of accepted and rejected points in phase 1 or 2
      if( !is.null(rej_evol.pdf) ) {
        points(x=x_star[!accept_1][accept_2],w[!accept_1][accept_2],col="gold",pch=19)
        points(x=x_star[!accept_1][!accept_2],y=w[!accept_1][!accept_2],col="red",pch=19)
      }
      # Add to abscissae those point evaluated in h(x)
      abscissae <- add_points.abscissae( abscissae, new_T_k, new_h_T, new_hp_T )
      abscissae <- get_zi( abscissae )
      #check( abscissae )
    }
    
  }

  sim_values <- sim_values[1:B]
  
  if( !is.null(rej_evol.pdf) ) {
    dev.off(dev_rej)
  }
  if( !is.null(abs_evol.pdf) ) {
    dev.off(dev_abs)
  }
  
  if( !is.null(hist.pdf) ) {
    pdf(file=hist.pdf,width=10, height=7, onefile=T)
    hist(sim_values,col="grey95",breaks=20)
    dev.off()
  }
  suppressWarnings( par(def.par) ) # reset plot settings to default
  
  return( sim_values )
}
