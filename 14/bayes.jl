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

function plotpost(post;plot_hpd=true,c_main="cornflowerblue",c_hpd="grey",showplot=true)
  # Take a function argument "post" which is an array of 
  # posterior draws and plots the posterior density
  # To do: Add vertical line: Posterior mean
  #        http://spencerlyon.com/PlotlyJS.jl/examples/shapes/
  k = kde(post)
  a = attr(zeroline=false,showgrid=false)
  l = Layout(yaxis=a, xaxis=a, showlegend=false)
  p = scatter(x=k.x, y=k.density, name="posterior", marker=attr(color=c_main),
              fill=:tozeroy, line_width=0)
  if plot_hpd
    lower, upper = hpd(post)
    ind = lower .< k.x .< upper
    k_x_trunc = k.x[ind]
    k_y_trunc = k.density[ind]
    g = scatter(x=k_x_trunc, y=k_y_trunc, name="hpd", marker=attr(color=c_hpd),
                fill=:tozeroy,line_width=0)
    p = [p, g]
  end

  showplot ? out=plot(p,l) : out=(p,l)
  out
end




function plotposts(M; spacing=.1, plot_hpd=true,c_main="cornflowerblue",c_hpd="grey")
  n = size(M)[1]
  l = Dict{AbstractString,Any}()
  #l = Dict{Symbol,Any}()
  steps = linspace(0,1,n+1)
  s = Array{PlotlyJS.GenericTrace{Dict{Symbol,Any}},1}(n*n)

  function makeplot(i,j)
    ind = (i-1)*n + j
    xi = string("x",ind)
    yi = string("y",ind)

    if i < j
      l[string("xaxis",ind)] = Dict("domain"=>[steps[i]+spacing,steps[i+1]-spacing],
                                    "anchor"=>string("y",ind))
      l[string("yaxis",ind)] = Dict("domain"=>[steps[j]+spacing,steps[j+1]-spacing])
    else
      l[string("xaxis",ind)] = Dict("domain"=>[steps[i]+spacing,steps[i+1]-spacing])
      l[string("yaxis",ind)] = Dict("domain"=>[steps[j]+spacing,steps[j+1]-spacing],
                                    "anchor"=>string("x",ind))
    end

    if ind == n
      l[string("xaxis",ind)] = Dict("domain"=>[steps[i]+spacing,steps[i+1]-spacing],
                                    "anchor"=>string("y",ind))
      l[string("yaxis",ind)] = Dict("domain"=>[steps[j]+spacing,steps[j+1]-spacing],
                                    "anchor"=>string("x",ind))
    end


    out = scatter(x=randn(5),y=randn(5),xaxis=xi,yaxis=yi)
    #if i==j
    #  out = scatter(x=randn(5),y=randn(5),xaxis=xi,yaxis=yi)
    #else
    #  out = scatter(x=randn(5),y=randn(5),xaxis=xi,yaxis=yi)
    #end

    out
  end

  #diag_plot = [plotpost(collect(M[i,:]),showplot=false) for i in 1:n]
  #s = [makeplot(i,j) for i in 1:n, j in 1:n]
  for i in 1:n, j in 1:n
    s[(i-1)*n + j] = makeplot(i,j)
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
x = randn(100)
y = randn(100)
r = round(cor(x,y),3)

k1 = kde(x)
k2 = kde(y)
p11 = plot(scatter(x=k1.x,y=k1.density,marker=attr(color=:orange)))
p22 = plot(scatter(x=k2.x,y=k2.density,marker=attr(color=:orange)))


a = attr(zeroline=false,showticklabels=false,showgrid=false)
p12 = plot(scatter(x=[0],y=[0],mode="text",text=r,textfont_size=10+40*abs(r)),
Layout(xaxis=a,yaxis=a))

h = histogram2dcontour(x=x,y=y,showscale=false,reversescale=true,colorscale=:Hot)
p21 = plot(h)

p = [p11 p12; 
     p21 p22]
p.plot.layout["bargap"] = .1
p.plot.layout["showlegend"] = false
p.plot.layout["width"] = 650
p.plot.layout["height"] = 650
p

pp = [p21, p21]

=#


#=
include("bayes.jl")
using Distributions
Bayes.plotpost(randn(100))
tmp = Bayes.plotposts(randn(4,100),spacing=.01)

using PlotlyJS
s1 = scatter(x=randn(3),y=randn(3),xaxis=:x1,yaxis=:y1,text=:yes1)
s2 = scatter(x=randn(3),y=randn(3),xaxis=:x2,yaxis=:y2,text=:yes2)
s3 = scatter(x=randn(3),y=randn(3),xaxis=:x3,yaxis=:y3,text=:yes3)
s4 = scatter(x=randn(3),y=randn(3),xaxis=:x4,yaxis=:y4,text=:yes4)
ll = Layout(xaxis1=attr(           zeroline=false,domain=[0, .45]),
            xaxis2=attr(           zeroline=false,domain=[.5, .99]),
            xaxis3=attr(anchor=:y3,zeroline=false,domain=[0, .45]),
            xaxis4=attr(anchor=:y4,zeroline=false,domain=[.5, .99]),
            yaxis1=attr(           zeroline=false,domain=[0, .45]),
            yaxis2=attr(anchor=:x2,zeroline=false,domain=[0, .45]),
            yaxis3=attr(           zeroline=false,domain=[.5, .99]),
            yaxis4=attr(           zeroline=false,domain=[.5, .95]))
tmp2 = plot([s1, s2, s3, s4],ll)
ll2 = Layout(xaxis1=attr(zeroline=false,domain=[0, 1]),
             xaxis2=attr(zeroline=false,domain=[0, 1]),
             yaxis1=attr(zeroline=false,domain=[0,.45]),
             yaxis2=attr(zeroline=false,domain=[.5, 1]))
p = plot([s1, s2],ll2)

M = randn(2,100)
plot([s1,s2],Layout(layout))
=#
