logdet <- function(V) determinant(V,log=TRUE)$mod[1]
tr <- function(M) sum(diag(M))

rwishart <- function(v,P) { # P = Precision Matrix
  k <- nrow(P)
  ii <- 1:k
  A <- matrix(0,k,k)
  diag(A) <- sqrt(rchisq(k,v-ii+1))
  ltA <- lower.tri(A)
  #A[ltA] <- rnorm(sum(ltA))
  A[ltA] <- rnorm((k-1)*k/2)
  U <- chol(P)
  L <- t(U)
  LA <- L %*% A
  LA %*% t(LA) # LAA'L'
}

# S = Scale Matrix. S.inv = Precision Matrix
riwishart <- function(v,S.inv) solve(rwishart(v,S.inv)) 

                     # L = Lower Tri = t(chol(S))
rmvnorm <- function(M,S,n=nrow(S)) M + t(chol(S)) %*% rnorm(n)

rniw <- function(m,s,r,S) {
  iW <- riwishart(r,S)
  rmvnorm(m,iW/s)
}

rniw.postpred <- function(m,s,r,S,brief=TRUE) {
  iW <- riwishart(r,S)
  mu <- rmvnorm(m,iW/s)
  postpred.y <- rmvnorm(mu,iW)

  if (brief) {
    out <- postpred.y
  } else {
    out <- list("mu"=mu,"S"=iW,"postpred"=postpred.y)
  }

  out
}

log.dniw <- function(Y,mu,V,m,s,r,S) {
  iV <- solve(V)
  n <- nrow(Y)
  k <- ncol(Y)
  c.rows <- t(apply(Y,1,function(y) y - mu))
  c.sum <- 0
  for (i in 1:nrow(c.rows)) c.sum <- c.sum + c.rows[i,] %*% iV %*% c.rows[i,]
  c.sum <- c(c.sum)
  log.dmvnorm <- -n/2 * logdet(V) - c.sum/2
  log.diw <- -(r+k+1)*.5*logdet(V) -.5*tr(S%*%iV)

  log.dmvnorm + log.diw 
}


#system.time({v <- 1000;  rwishert(v+1,diag(v))})

# CHECK:
#N <- 50000
#rr <- lapply(1:N, function(i) riwishart(5,diag(3)))
#r_sum <- matrix(0,3,3)
#for (r in rr) r_sum <- r_sum + r / N
#r_sum
#diag(3) / (5-3-1)
