module auxGibbs
using Distributions
export sample_t_hier

"""
function sample_t_hier(y ;B=2000, burn=100000-B, 
  init=Dict(:mu_vec=>zeros(length(y)), :sig2=>1, :mu=>1, :tau2=>1, :nu=>5),
  priors=Dict(:m=>0, :s2=>1, :a_tau=>3, :b_tau=>3, :a_sig=>3, :b_sig=>3))

Note: 
> yᵢ | λᵢ ~ Normal(μᵢ, (σ^²/√λᵢ))
> 
>      λᵢ ~  Gamma(v/2, v/2), shape and **rate**
> 
>   ⇒  yᵢ ~ tᵥ(μᵢ, σ), a location-scale t with df = v.

The Julia implementations are consistent with the wikipedia parameterizations.
"""
function sample_t_hier(y ;B=2000, burn=100000-B, 
  init=Dict(:mu_vec=>zeros(length(y)), :sig2=>1, :mu=>1, :tau2=>1, :nu=>5),
  priors=Dict(:m=>0, :s2=>1, :a_tau=>3, :b_tau=>3, :a_sig=>3, :b_sig=>3))

  nu = init[:nu]


  for i in 2:B
    
  end
  

end

end
