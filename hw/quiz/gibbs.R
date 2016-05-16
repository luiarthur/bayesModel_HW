source("../hw2/rwishert.R")

sample.niw <- function(Y,priors,B=1,printProgress=FALSE) {
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

  one.sample <- function(i) {
    if (printProgress && i%%(B/100)==0) cat("\rProgress: ",i,"/",B)
    rniw.postpred(m=m.post,s=s.post,r=r.post,iS=iS.post,brief=FALSE)
  }
  postpred.y <- lapply(as.list(1:B),one.sample)

  postpred.y
}

#postpred.y <- sample.niw(iris[,1:2],list("m"=apply(iris[,1:2],2,mean),"s"=1,"r"=10,"S"=diag(2)),B=1000)

gibbs.niw.hier <- function(Y,priors,B=10000,burn=B*.1) {
  # Hyper Priors
  m0 <- priors$m # prior mean vector for mu_i
  S0 <- priors$S # prior param for Sigma
  r0 <- priors$r
  v0 <- priors$v

  # Precomputes
  k <- ncol(Y)
  n <- nrow(Y)

  mu.3d <- array(0,c(n,k,B)) # row, col, slice
  S <- array(0,c(k,k,B)) # row, col, slice
  S[,,1] <- diag(k)
  mu <- matrix(0,B,k)

  for (b in 2:B) {
    # Update mu_i's
    for (i in 1:n) {
      tmp <- rmvnorm( (mu[b-1,]+Y[i,])/2, S[,,b-1]/2)
      mu.3d[i,,b] <- as.matrix(tmp)
    }
    
    # Update mu, Sigma
    priors.list <- list("m"=m0,"s"=v0,"S"=S0,"r"=r0)
    pp <- sample.niw(mu.3d[,,b],priors.list,B=1)
    mu[b,] <- pp[[1]]$mu
    S[,,b] <- pp[[1]]$S

    cat("\rProgress: ",b,"/",B)
  }
  
  list("mu.3d"=mu.3d,"S"=S,"mu"=mu)
}


