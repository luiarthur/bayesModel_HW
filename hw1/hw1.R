#install.packages("LearnBayes")
library(LearnBayes)
source("../R_Functions/plotPost.R",chdir=TRUE)
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

  #u <- ifelse(u < .0001,.0001,u)
  mu <- 1/ (exp(-u)+1)
  tau <- exp(v)
  
  v - 2*log(1+exp(v)) + sum(lbeta(mu*tau+y, n+tau-mu*tau-y) - lbeta(mu*tau,tau-mu*tau))
}

proposal <- function(current_param) mvrnorm(current_param,cand_S)
cand_S <- diag(2)
col.names <- c("logit(mu)","log(tau)")

param.transform <- function(uv) {
  if (!(is.matrix(uv))) {
    uv <- matrix(uv,nrow=1)
  }
  u <- uv[,1]
  v <- uv[,2]
  mu <- 1 / (exp(-u) + 1)
  tau <- exp(v)
  
  cbind(mu,tau)
}

out <- mh_multivariate(loglike_plus_logprior,proposal,cand_S,init=c(0,0),col.names)
plot.posts(out$post,names=col.names)
plot.posts(param.transform(out$post),names=c("mu","tau"))
#ext <- which(param.transform(out$post)[,1] > 5)
#plot.posts(param.transform(out$post)[-ext,],names=c("mu","tau"))



#########################################################################

opt <- optim(c(0,0),fn=function(x) -loglike_plus_logprior(x),hessian=TRUE)
s <- expand.grid(seq(-20,20,len=100), seq(-20,20,len=100))
ys <- apply(s,1,function(x) loglike_plus_logprior(x))
plotmap(ys,s,bks=c(-1000,-550))

(log_posterior_mode <- opt$par)
(log_posterior_cov <- solve(opt$hess)) # note that we don't need to negate again because the hessian is already negated because we are minimizing -loglike

h <- function(uv) {
  loglike_plus_logprior(uv) / -J
}

h.star <- function(uv) {
  v <- uv[2]
  h(uv) - log(uv) / J # ???
}

opt1 <- optim(c(0,0),fn=function(x) h(x),hessian=TRUE)
opt2 <- optim(c(0,0),fn=function(x) h.star(x),hessian=TRUE)

theta.star <- opt2$par
S.star <- opt2$hess
theta.hat <- opt1$par
S <- opt1$hess

(post.mean.laplace <- theta.star * sqrt(det(S.star)) * exp(-J * h.star(theta.star)) / (sqrt(det(S)) * exp(-J * h(theta.hat))))
