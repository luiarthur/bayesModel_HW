using Distributions

#=
  For the model:
  y | X, β, ϕ  ~ N( Xβ, 1/ϕ Iₙ)
        p(β,ϕ) ~ N(  0, g/ϕ (X'X)⁻¹ )

  By default, g is set to n (the number of observations).
  Note that g-priors work fine for n ≤ p

=#

function gprior(y, X, B; g=0, add_intercept=true, setseed=0)
  if setseed>0 srand(setseed) end
  X = add_intercept ? [fill(1,size(X)[1]) X] : X

  n,k = size(X)
  if g == 0  g = n end

  XX = X'X
  XXi = inv(XX)
  beta_hat = XXi * X'y
  c = g / (1+g)
  sse = sum( (y-X*beta_hat).^2 )
  phi_shape = (n-1)/2
  phi_rate = (sse./2 + beta_hat'*XX*beta_hat / (2*(1+g)))[1]

  samp_beta(phi) = rand(MultivariateNormal(c * beta_hat, c/phi * XXi))
  samp_phi(N) = rand(Gamma( phi_shape, 1/phi_rate ),N)

  new_phi = samp_phi(B)
  tmp_beta = [samp_beta(i) for i in new_phi]
  new_beta = hcat([x for x in tmp_beta]...)'

  Dict(:phi=>new_phi, :beta=>new_beta)
end
