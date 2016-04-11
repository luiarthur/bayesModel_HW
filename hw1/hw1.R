#install.packages("LearnBayes")
library(LearnBayes)
source("plotmap.R")
source("mh.R")

mvrnorm <- function(M,S,n=nrow(S)) M + t(chol(S)) %*% rnorm(n)

dat <- cancermortality

y <- dat$y
n <- dat$n
J <- nrow(dat)


loglike_plus_logprior <- function(uv) {
  lbeta <- function(a,b) lgamma(a) + lgamma(b) - lgamma(a+b)
  u <- uv[1]
  v <- uv[2]

  u <- ifelse(u < .0001,.0001,u)
  mu <- 1/ (exp(-u)+1)
  tau <- exp(v)
  
  v - 2*log(1+exp(v)) + sum(lbeta(mu*tau+y, n+tau-mu*tau-y) - lbeta(mu*tau,tau-mu*tau))
}

proposal <- function(current_param) mvrnorm(current_param,cand_S)
cand_S <- diag(2)

source("mh.R")
out <- mh_multivariate(loglike_plus_logprior,proposal,cand_S,init=c(0,0))







#########################################################################

optim(c(-7,7),fn=function(x) -loglike_plus_logprior(x),hessian=TRUE)
s <- expand.grid(seq(-10,10,len=100), seq(0,100,len=100))
ys <- apply(s,1,function(x) -loglike_plus_logprior(x))
plotmap(ys,s)


h <- function(uv) {
  loglike_plus_logprior(uv) / -J
}

h.star <- function(uv) {
  v <- uv[2]
  h(uv) - (- v - 2*log(1+exp(v))) / J
}


s1 <- expand.grid(seq(-10,10,len=100), seq(0,100,len=100))
ys1 <- apply(s1,1,function(x) h(x))
plotmap(ys1,s1)
optim(c(7,20),fn=function(x) h(x),hessian=TRUE)

s2 <- expand.grid(seq(-10,10,len=100), seq(0,100,len=100))
ys2 <- apply(s2,1,function(x) h.star(x))
plotmap(ys1,s1)
optim(c(7,7),fn=function(x) h.star(x),hessian=TRUE)
