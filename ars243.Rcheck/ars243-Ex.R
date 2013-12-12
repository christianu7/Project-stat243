pkgname <- "ars243"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ars243')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("S")
### * S

flush(stderr()); flush(stdout())

### Name: S
### Title: Cumulative density function of normalized exponentiated upper
###   hull function
### Aliases: S
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
x<-c(-1,3)
S(x , T_k, h_T , hp_T , z_i)



cleanEx()
nameEx("S_inv")
### * S_inv

flush(stderr()); flush(stdout())

### Name: S_inv
### Title: Inverse of the Cumulative Distribution of the exponentiated
###   envelope function
### Aliases: S_inv
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
abscissae <- list(T_k=T_k, h_T=h_T, hp_T=hp_T, z_i=z_i)
class(abscissae) <- "abscissae"
x<-c(0.5,0.95)
S_inv(x , abscissae)



cleanEx()
nameEx("add_points.abscissae")
### * add_points.abscissae

flush(stderr()); flush(stdout())

### Name: add_points.abscissae
### Title: Update the abscissae set
### Aliases: add_points.abscissae
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
x <- list(T_k=T_k, h_T=h_T, hp_T=hp_T)
class(x) <- "abscissae"
new_T_k <- c(-5,5)
new_h_T<-log(dnorm(new_T_k))
hp<-function(x){-x}
new_hp_T<-hp(new_T_k)
add_points.abscissae(x = x, new_T_k = new_T_k, new_h_T = new_h_T, new_hp_T = new_hp_T)



cleanEx()
nameEx("ars")
### * ars

flush(stderr()); flush(stdout())

### Name: ars
### Title: Adaptive Rejection Sampling
### Aliases: ars

### ** Examples

f<- function(x){dnorm(x,5,2.5)} 
ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='NO')  #no pdf displayed

ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='test.pdf')  #pdf saved to working directory

ars(B=100, f=f ,l_f=-Inf, u_f=Inf, ep=1e-10 , m=10, evol.pdf='C:\Users\User\Documents\test.pdf')  #pdf saved to specific directory



cleanEx()
nameEx("as.abscissae")
### * as.abscissae

flush(stderr()); flush(stdout())

### Name: as.abscissae
### Title: Generating Output of Summary Information.
### Aliases: as.abscissae

### ** Examples

   ## Generic data generation
l_f <- 1e-5
u_f <- 1e5
h <- function(x){dnorm(mu,sigma)}
abscissae <- as.abscissae(x=seq(l_f+0.01,u_f-0.01,length.out=4), h, l_h=l_f, u_h=u_f, ep=1e-8)



cleanEx()
nameEx("bucket")
### * bucket

flush(stderr()); flush(stdout())

### Name: bucket
### Title: Bucket Function.
### Aliases: ' bucket'

### ** Examples

   myBuckets <- c(1, 3, 6, 9)
   bucket(4, myBuckets)



cleanEx()
nameEx("check.abscissae")
### * check.abscissae

flush(stderr()); flush(stdout())

### Name: check.abscissae
### Title: Generation of Initial X Values for ARS.
### Aliases: 'check.abscissae '

### ** Examples

 f <- function(x) { dnorm(x, 5, 2.5) }
	h <- function(x) { log(f(x)) }
	x <- c(-10, 1, 4, 8)
	check.abscissae(x=x, h=h, l_h=-12, u_h=10, ep=1e-3)



cleanEx()
nameEx("deriv_num")
### * deriv_num

flush(stderr()); flush(stdout())

### Name: deriv_num
### Title: Numeric Differentiation of Functions or Expressions.
### Aliases: deriv_num

### ** Examples

## Normal Distribution
mu <- 5
sigma <- 2
f <- function(x) {dnorm(x,mu,sigma)}
der <- deriv_num(f, 4.5)
## User Defined Function
f <- function(x) { quote(1/(sigma*sqrt(2*pi)) * exp(-(x-mu)^2/(2*sigma^2))) } 
der <- deriv_num(f, 4.5, 1e-3)



cleanEx()
nameEx("get_zi")
### * get_zi

flush(stderr()); flush(stdout())

### Name: get_zi
### Title: Compute Intersection Points of Derivatives.
### Aliases: ' get_zi'

### ** Examples

testInput <- list(T_k=c(-10,1,4,8), h_T=c(-19, -3, -2, -2.5), hp_T=c(2, 0.5, 0.1, -0.5), z_i=NULL, k=4, l_h=-12, u_h=10)
get_zi.abscissae(testInput)



cleanEx()
nameEx("int_s")
### * int_s

flush(stderr()); flush(stdout())

### Name: int_s
### Title: Integral of exponentiated upper hull function
### Aliases: int_s
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
int_s(T_k, h_T, hp_T, z_i)



cleanEx()
nameEx("l")
### * l

flush(stderr()); flush(stdout())

### Name: l
### Title: calculate the squeezing function
### Aliases: l
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(0,3,5,6,10)
h_T<-log(dnorm(T_k))
x<-c(-1,2)
l(x=x,T_k=T_k,h_T=h_T)



cleanEx()
nameEx("permdiff")
### * permdiff

flush(stderr()); flush(stdout())

### Name: permdiff
### Title: Permutation Test (Non-Parametric Test)
### Aliases: permdiff

### ** Examples

x <- rnorm(30)
y <- runif(30, min=-1, max=1)
permdiff(x=x, y=y, 100)



cleanEx()
nameEx("plot.abscissae")
### * plot.abscissae

flush(stderr()); flush(stdout())

### Name: plot.abscissae
### Title: Plot upper and lower function and sampling PDF and CDF
### Aliases: plot.abscissae
### Keywords: ~kwd1 ~kwd2

### ** Examples

l_f<-100
u_f <- 100
eps <- 0.1
T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
abscissae <- list(T_k=T_k, h_T=h_T, hp_T=hp_T, z_i=z_i)
class(abscissae) <- "abscissae"
plot.abscissae(abscissae, plot.h=F)



cleanEx()
nameEx("s_pdf")
### * s_pdf

flush(stderr()); flush(stdout())

### Name: s
### Title: Normalized exponentiated envelope function.
### Aliases: s
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
x<-c(-1,3)
s( x=x , T_k=T_k , h_T=h_T , hp_T=hp_T , z_i=z_i )



cleanEx()
nameEx("u")
### * u

flush(stderr()); flush(stdout())

### Name: u
### Title: Calculate the envelop function value
### Aliases: u
### Keywords: ~kwd1 ~kwd2

### ** Examples

T_k<-c(-3,-1,1,3)
h_T<-log(dnorm(T_k))
hp<-function(x){-x}
hp_T<-hp(T_k)
z_i<-c(-Inf,-2, 0, 2,Inf)
x<-c(-1,3)
u( x=x , T_k=T_k , h_T=h_T , hp_T=hp_T , z_i=z_i )



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
