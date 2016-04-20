source("../hw2/rwishert.R")

sample.niw <- function(Y,priors,B=1) {
  # Hyper Priors
  m0 <- priors$m # prior mean vector for mu_i
  S0 <- priors$S # prior param for Sigma
  s0 <- priors$s
  r0 <- priors$r

  # Precomputes
  k <- ncol(Y)
  n <- nrow(Y)
  Y.bar <- apply(Y,2,mean)
  c.rows <- t(apply(Y,1,function(y) y - Y.bar))
  C.mat <- matrix(0,k,k)
  for (i in 1:n) {
    C.mat <- C.mat + c.rows[i,] %*% t(c.rows[i,])
  }

  m.post <- (s0*m0 + n*Y.bar) / (s0+n) 
  s.post <- s0+n
  r.post <- r0+n
  S.post <- solve(S0) + C.mat + s0*n/(s0+n) * (Y.bar-m0) %*% t(Y.bar-m0)
  iS.post <- solve(S.post) # IMPORTANT!!!

  postpred.y <- lapply(as.list(1:B),function(i) rniw.postpred(m=m.post,s=s.post,r=r.post,iS=iS.post,brief=FALSE))

  postpred.y
}

#postpred.y <- sample.niw(iris[,1:2],list("m"=apply(iris[,1:2],2,mean),"s"=1,"r"=10,"S"=diag(2)),B=1000)

gibbs.niw.hier <- function(Y,priors,B=10000,burn=B*.1) {
  # Hyper Priors
  m0 <- priors$m # prior mean vector for mu_i
  S0 <- priors$S # prior param for Sigma
  r0 <- prior$r
  v0 <- prior$v

  # Precomputes
  k <- ncol(Y)
  n <- nrow(Y)

  mu.list <- as.list(1:n) 
  #lapply(as.list(1:n),function(i) matrix(0,B,k))

  #for (b in 2:B) {
  #  for (i in 1:n) {
  #     mu.pp.list <- sample.niw(mu,priors)
  #  }
  #}
 
}


