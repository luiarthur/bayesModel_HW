# b) Multivariate proposal for logit(mu), log(tau) --- easy
mh_multivariate <- function(log_lik_plus_prior, propose, cand_S, init, col.names=NULL, B=10000, burn=1000) {

  p <- ncol(cand_S)

  # Initialize
  posterior_draws <- matrix(0,B+burn,p)
  if (!is.na(col.names[1])) colnames(posterior_draws) <- col.names
  posterior_draws[1,] <- init
  acceptance_rate <- 0

  for (it in 2:(B+burn)) {
    cand <- propose(posterior_draws[it-1,])
    acceptance_ratio <- log_lik_plus_prior(cand) - 
                        log_lik_plus_prior(posterior_draws[it-1,])
    if (acceptance_ratio > log( runif(1) )) {
      posterior_draws[it,] <- cand
      if (it > burn) acceptance_rate <- acceptance_rate + 1
    } else {
      posterior_draws[it,] <- posterior_draws[it-1,]
    }
    cat("\rProgress: ",it,"/",B+burn)
  }
  cat("\nAcceptance Ratio: ", acceptance_rate/B,"\n")
  

  settings <- list("cand_S"=cand_S,"init"=init,head(posterior_draws,burn))
  list("posterior"=tail(posterior_draws,B), "accept"=acceptance_rate/B, "settings"=settings)
}

