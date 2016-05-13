module auxGibbs
using Distributions
export sample_t_hier, postpred_t_hier, sample_Normal, deviance_t_hier, deviance_simple

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
      rand(Gamma( (nu+1)/2, 1/(nu/2 + (y[i]-mu_vec_curr[i])^2 / 2sig2_curr)))

    [updateLambda_at(i) for i in 1:n]
  end

  for j in 1:B+burn
    i = j<=burn? 1 : j-burn
    i_prev = i<=2? 1 : i-1

    sig2[i] = updateSig2(mu_vec[:,i_prev], lambda)
    tau2[i] = updateTau2(mu_vec[:,i_prev], mu[i_prev])
    mu[i] = updateMu(mu_vec[:,i_prev], tau2[i])
    mu_vec[:,i] = updateMuVec(mu[i], lambda, sig2[i], tau2[i])
    lambda = updateLambda(mu_vec[:,i], sig2[i])

    if (j) % ((B+burn)/10) == 0 print("\rProgress: ",j,"/",B+burn) end
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

  for b in 1:B, i in 1:n
    pp[b,i] = rand(TDist(nu)) * sqrt(sig2[b]) + mu_v[b,i]
  end

  pp
end


"""
Gibbs sampler for 
> yᵢ | μ  ~ Normal(μ, σ^²)
>
>      μ  ~ Normal(m, s²)
>
>      σ² ~ IG(a,b)

function sample_Normal(y; B=2000, burn=Int(round(B*.3)), 
                       init=Dict(:mu=>2.0,:sig2=>1.0),
                       priors=Dict(:m=>2,:s2=>1,:a=>3,:b=>3))
"""
function sample_Normal(y; B=2000, burn=Int(round(B*.3)), 
                       init=Dict(:mu=>2.0,:sig2=>1.0),
                       priors=Dict(:m=>2,:s2=>1,:a=>3,:b=>3))

  s2 = priors[:s2]
  m = priors[:m]
  a = priors[:a]
  b = priors[:b]
  sum_y = sum(y)
  n = length(y)

  function updateMu(sig2_curr)
    denom_new = s2*n + sig2_curr
    numer_new = s2*sum_y + sig2_curr*m
    m_new = numer_new / denom_new
    v_new = s2*sig2_curr / denom_new
    rand(Normal(m_new,sqrt(v_new)))
  end

  sig2 = fill(init[:sig2],B)
  mu = fill(init[:mu],B)

  for j in 1:B+burn
    i = j<=burn? 1 : j-burn
    i_prev = i<=2? 1 : i-1

    mu[i] = updateMu(sig2[i_prev])
    sig2[i] = rand(InverseGamma(a+n/2, b+sum((y-mu[i]).^2)/2))

    if j % ((B+burn)/10) == 0 print("\rProgress: ",j,"/",B+burn) end
  end
  
  Dict(:mu=>mu, :sig2=>sig2)
end

function postpred_Normal(samps)
  n = length(samps[:mu])
  [rand(Normal(samps[:mu][i], samps[:sig2][i])) for i in 1:n]
end

function deviance_t_hier(mu_vec_post, sig2_post, y, nu)
  B = length(sig2_post)
  n = length(y)

  lf(mi,ss,yi) = logpdf(TDist(nu),(yi-mi)/sqrt(ss)) - .5log(ss)
  D(mb,ss) = -2sum([lf(mb[i],ss,y[i]) for i in 1:n])
  D_bar = mean([D(mu_vec_post[b,:],sig2_post[b]) for b in 1:B])
  2D_bar - D(mean(mu_vec_post,1),mean(sig2_post))
end

function deviance_simple(mu_post,sig2_post,y)
  n = length(y)
  B = length(sig2_post)
  D(m,s2) = -2sum(logpdf(Normal(m,sqrt(s2)), y))
  D_bar = mean([D(mu_post[b],sig2_post[b]) for b in 1:B])
  2D_bar - D(mean(mu_post),mean(sig2_post))
end

end
