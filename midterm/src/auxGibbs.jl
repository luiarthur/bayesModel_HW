module auxGibbs
using Distributions
export sample_t_hier, postpred_t_hier

"""
function sample_t_hier(y ;B=2000, burn=100000-B, 
  init=Dict(:mu_vec=>zeros(Float64,length(y)), 
            :sig2=>1.0, :mu=>1.0, :tau2=>1.0, :nu=>5.0, 
            :lambda=>ones(Float64,length(y))),
  priors=Dict(:m=>0.0, :s2=>1.0, :a_tau=>3.0, :b_tau=>3.0, 
              :a_sig=>3.0, :b_sig=>3.0))

Perform Gibbs sampling for a t-distributed model by introducing an auxiliary 
variable, λᵢ distributed Gamma apriori. The collapsed version of the model yields
a t-distributed model. In statistical notation,

Note: 
> yᵢ | λᵢ ~ Normal(μᵢ, (σ^²/√λᵢ))
> 
>      λᵢ ~  Gamma(v/2, v/2), shape and **rate**
> 
>   ⇒  yᵢ ~ tᵥ(μᵢ, σ), a location-scale t with df = v.

Note that the Gamma distribution in Julia is parameterized with shape and **scale**.
To sample from a Gamma(shape=a, rate=b), do:

```julia
rand(Gamma(a,1/b))
```

The Inverse Gamma in Julia has the same parameterization as on Wikipedia (shape and scale).
"""
function sample_t_hier(y ;B=2000, burn=100000-B, 
  init=Dict(:mu_vec=>ones(Float64,length(y)), 
            :sig2=>1.0, :mu=>1.0, :tau2=>1.0, :nu=>5.0, 
            :lambda=>ones(Float64,length(y))),
  priors=Dict(:m=>0.0, :s2=>1.0, :a_tau=>3.0, :b_tau=>3.0, 
              :a_sig=>3.0, :b_sig=>3.0))

  # Initialize:
  n = length(y)
  nu = init[:nu]
  lambda = init[:lambda]

  # Preallocate storage:
  mu_vec = hcat(fill(collect(init[:mu_vec]),B)...)
  sig2 = fill(init[:sig2],B)
  mu = fill(init[:mu],B)
  tau2 = fill(init[:tau2],B)
  
  # Set priors:
  m = priors[:m]
  s2 = priors[:s2]
  a_tau = priors[:a_tau]
  b_tau = priors[:b_tau]
  a_sig = priors[:a_sig]
  b_sig = priors[:b_sig]

  updateSig2(mu_vec_curr,lambda_curr) =
    rand(InverseGamma(a_sig+n/2, b_sig+sum((y-mu_vec_curr).^2 .*lambda_curr)/2))

  updateTau2(mu_vec_curr,mu_curr) = 
    rand(InverseGamma(a_tau+n/2, b_tau+sum((mu_vec_curr-mu_curr).^2)/2))

  function updateMu(mu_vec_curr,tau2_curr)
    denom_new = s2*n + tau2_curr
    numer_new = s2*sum(mu_vec_curr) + tau2_curr*m
    m_new = numer_new / denom_new
    v_new = s2*tau2_curr / denom_new
    rand(Normal(m_new,sqrt(v_new)))
  end

  function updateMuVec(mu_curr,lambda_curr,sig2_curr,tau2_curr)
    function updateMuVec_at(i)
      vi2 = sig2_curr / lambda_curr[i]
      denom_new = tau2_curr + vi2
      numer_new = y[i]*tau2_curr + mu_curr*vi2
      m_new = numer_new / denom_new
      v_new = tau2_curr*vi2 / denom_new
      rand(Normal(m_new,sqrt(v_new)))
    end

    [updateMuVec_at(i) for i in 1:n]
  end

  function updateLambda(mu_vec_curr, sig2_curr)
    updateLambda_at(i) = 
      rand(Gamma( (nu+1)/2, nu/2 + (y[i]-mu_vec_curr[i])^2 / 2sig2_curr ))

    [updateLambda_at(i) for i in 1:n]
  end

  for j in 1:B+burn-2
    i = j<burn? 1 : j-burn+2
    i_prev = i==1? 1 : i-1

    sig2[i] = updateSig2(mu_vec[:,i_prev], lambda)
    tau2[i] = updateTau2(mu_vec[:,i_prev], mu[i_prev])
    mu[i] = updateMu(mu_vec[:,i_prev], tau2[i])
    mu_vec[:,i] = updateMuVec(mu[i], lambda, sig2[i], tau2[i])
    lambda = updateLambda(mu_vec[:,i], sig2[i])

    if (j+2) % ((B+burn)/10) == 0 print("\rProgress: ",j+2,"/",B+burn) end
  end
  
  return Dict(:sig2=>sig2, :tau2=>tau2, :mu=>mu, :mu_vec=>mu_vec', 
              :lambda=>lambda, :priors=>priors, :init=>init, :B=>B, :burn=>burn)
end

function postpred_t_hier(samps)
  sig2 = samps[:sig2]
  mu_v = samps[:mu_vec]
  nu = samps[:init][:nu]
  B = samps[:B]
  n = size(mu_v)[2]
  pp = zeros(Float64,B,n)
  
  #[rand( Normal(mu_v[b,i],  sqrt(sig2[b]/rand(Gamma(nu/2,2/nu))) )) for b in 1:B, i in 1:n]
  for b in 1:B
    for i in 1:n
      #mi = mu_v[b,i]
      #lam_i = rand(Gamma(nu/2,2/nu))
      #si = sqrt( sig2[b] / lam_i )
      #pp[b,i] = rand(Normal(mi,si))
      pp[b,i] = rand(TDist(5)) * sqrt(sig2[b]) + mu_v[b,i]
    end
  end
  pp
end

end
