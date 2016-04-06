#install.packages("LearnBayes")
library(LearnBayes)
source("plotmap.R")

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
  
  v - 2*log(1+exp(v)) + sum(lbeta(mu*tau+y, tau-mu*tau+n+y) - lbeta(mu*tau,tau-mu*tau))
}

optim(c(-10,10),fn=function(x) -loglike_plus_logprior(x),hessian=TRUE)
s <- expand.grid(seq(-10,10,len=100), seq(0,100,len=100))
ys <- apply(s,1,function(x) -loglike_plus_logprior(x))
plotmap(ys,s)
