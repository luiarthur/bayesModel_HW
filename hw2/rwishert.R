rwishart <- function(v,S) {
  k <- nrow(S)
  ii <- 1:k
  A <- matrix(0,k,k)
  diag(A) <- sqrt(rchisq(k,v-ii+1))
  ltA <- lower.tri(A)
  A[ltA] <- rnorm(sum(ltA))
  L <- chol(S)
  LA <- L %*% A
  LA %*% t(LA)
}
rmvnorm <- function(M,S,n=nrow(S)) M + t(chol(S)) %*% rnorm(n)

rniw <- function(m,s,r,S) {
  W <- rwishart(r,S)
  iW <- solve(W)
  rmvnorm(m,iW/s)
}

#riwishert <- function(v,S) solve(rwishert(v,S))
#system.time({v <- 1000;  rwishert(v+1,diag(v))})
