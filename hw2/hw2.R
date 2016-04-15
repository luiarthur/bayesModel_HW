source("rwishert.R")

# Sufficient Stats:
Y <- as.matrix(iris[,1:4])
Y.bar <- apply(Y,2,mean)
c.rows <- t(apply(Y,1,function(y) y - Y.bar))
C.mat <- matrix(0,4,4)
for (i in 1:nrow(c.rows)) {
  C.mat <- C.mat + c.rows[i,] %*% t(c.rows[i,])
}

# Prior Params:
s <- 1
m <- Y.bar
r <- 10
k <- length(Y.bar)
n <- nrow(Y)
S <- diag(k)

# Posterior Params:
m.post <- (s*m + n*Y.bar) / (s+n) 
s.post <- s+n
r.post <- r+n
S.post <- S + C.mat + s*n/(s+n) * Y.bar %*% t(Y.bar)

out <- t(sapply(1:1000,function(i)rniw(m.post,s.post,r.post,S.post)))
apply(out,2,quantile,c(.025,.5,.975))
