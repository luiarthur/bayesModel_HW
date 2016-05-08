module Bayes
using PlotlyJS, KernelDensity
export plotpost, hpd, plotposts

function hpd(x ;a=.05,len=1e3)
  A = linspace(0,a,len)
  quants = [quantile(x,[i,i+1-a]) for i in A]
  #d = map(q -> diff(q)[1], quants)
  d = pmap(q -> diff(q)[1], quants)
  quants[indmin(d)]
end

function plotpost(post;plot_hpd=true,c_main="cornflowerblue",c_hpd="grey",showplot=true,xaxis=0,yaxis=0,name="",title=name)
  # Take a function argument "post" which is an array of 
  # posterior draws and plots the posterior density
  # To do: Add vertical line: Posterior mean
  #        http://spencerlyon.com/PlotlyJS.jl/examples/shapes/
  k = KernelDensity.kde(post)
  a = attr(zeroline=false,showgrid=false)
  l = Layout(yaxis=a, xaxis=a, showlegend=false,title=title)
  p = scatter(x=k.x, y=k.density, name=name, marker=attr(color=c_main),
              fill=:tozeroy, line_width=0)

  if xaxis != 0
    restyle!(p,xaxis=xaxis)
  end
  if yaxis != 0
    restyle!(p,yaxis=yaxis)
  end

  if plot_hpd
    lower, upper = hpd(post)
    ind = lower .< k.x .< upper
    k_x_trunc = k.x[ind]
    k_y_trunc = k.density[ind]
    g = scatter(x=k_x_trunc, y=k_y_trunc, name="hpd", marker=attr(color=c_hpd),
                fill=:tozeroy,line_width=0)
    if xaxis != 0
      restyle!(g,xaxis=xaxis)
    end
    if yaxis != 0
      restyle!(g,yaxis=yaxis)
    end
    p = [p, g]
  end

  showplot ? out=plot(p,l) : out=(p,l)
  out
end




function plotposts(M; spacing=.1, plot_hpd=true,c_main="cornflowerblue",c_hpd="grey",names=fill("Posterior",size(M)[2]))
  n = size(M)[2]
  l = Dict{AbstractString,Any}()
  #l = Dict{Symbol,Any}()
  steps = linspace(0,1,n+1)
  #s = Array{PlotlyJS.GenericTrace{Dict{Symbol,Any}},1}(n*n)
  s = PlotlyJS.GenericTrace{Dict{Symbol,Any}}[]

  function makeplot(i,j)
    ind = (i-1)*n + j
    xi = string("x",ind)
    yi = string("y",ind)

    l[string("xaxis",ind)] = Dict("domain"=>[steps[i]+spacing,steps[i+1]-spacing],
                                  "anchor"=>string("y",ind),
                                  "zeroline"=>false,"showgrid"=>false)
    l[string("yaxis",ind)] = Dict("domain"=>[steps[j]+spacing,steps[j+1]-spacing],
                                  "anchor"=>string("x",ind),
                                  "zeroline"=>false,"showgrid"=>false)

    if i==j
      gl = plotpost(collect(M[:,i]),xaxis=xi,yaxis=yi,showplot=false,name=names[i])
      out = gl[1]
    elseif i<j
      out = histogram2dcontour(x=collect(M[:,i]),y=collect(M[:,j]),showscale=false,
                               xaxis=xi,yaxis=yi,
                               reversescale=true,colorscale=:Hot)
    else
      r = round(cor(collect(M[:,i]),collect(M[:,j])),3)
      ldx = l[string("xaxis",ind)]
      ldy = l[string("yaxis",ind)]
      ldy["showticklabels"] = ldx["showticklabels"] = false
      l[string("xaxis",ind)] = ldx
      l[string("yaxis",ind)] = ldy
      out = scatter(x=[0],y=[0],mode="text",text=r,textfont_size=10+40*abs(r),
                    xaxis=xi,yaxis=yi)
    end

    out
  end

  for i in 1:n, j in 1:n
    mp = makeplot(i,j)
    try
      for k in mp
        push!(s,k)
      end
    catch
      push!(s,mp)
    end
  end

  m = 30
  p = plot(s,Layout(l))
  p.plot.layout[:width] = 800
  p.plot.layout[:height] = 800
  p.plot.layout[:margin] = Dict(:t=>m, :b=>m, :r=>m, :l=>m)
  p.plot.layout[:showlegend] = false
  p
end

end


#=
include("bayes.jl")
Bayes.plotposts(randn(100,4),spacing=.03,names=["I","II","III","IV"])
Bayes.plotpost(randn(100),name="yup")
=#
