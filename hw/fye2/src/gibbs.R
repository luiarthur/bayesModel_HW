# gibbs.R
gibbs <- function(y, V, params=list(siga=3, sigb=1), 
                  init=list(mu=.5, 
                            th=rep(.5,length(y)), 
                            sig2=1),
                  B=1000, burn=1000) {

  stopifnot(length(y) == length(V))
  
  # init Gibbs
  K <- length(y)
  out <- matrix(0,B+burn,2+K)
  out[1,] <- c(init$mu, init$sig2, init$th)
  siga <- params$siga
  sigb <- params$sigb

  # start Gibbs
  for (i in 2:(B+burn)) {
    # update mu:
    th.curr <- out[i-1,-c(1:2)]
    s2.curr <- out[i-1,2]
    out[i,1] <- rnorm(1, mean(th.curr), sqrt(s2.curr/K))

    # update sig2
    mu.curr <- out[i,1]
    shp <- siga + K/2
    rate <- sum( (th.curr-mu.curr)^2 )/2 + sigb
    out[i,2] <- 1/rgamma(1, shp, rate)

    # update theta
    s2.curr <- out[i,2]
    for (j in 1:K) {
      m <- (y[j]*s2.curr+V[j]*mu.curr) / (s2.curr+V[j])
      v <- s2.curr*V[j] / (s2.curr + V[j])
      out[i,j+2] <- rnorm(1, m, sqrt(v))
    }
  }

  colnames(out) <- c("mu","sig2",paste0("theta",1:K))
  tail(out,B)
}
