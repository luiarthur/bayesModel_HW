# Arthur Lui's takehome midterm for AMS 207
# Review: BDA3 p. 294

using Distributions, RCall, DataFrames
import Bayes.hpd
import PlotlyJS
include("auxGibbs.jl")

R"source('../../quiz/mypairs.R',chdir=TRUE)"
plotposts = R"simple.plot.posts"
plotpost = R"simple.plot.post"
adderrbar = R"add.errbar"
density = R"density"
@rlibrary(graphics)
@rlibrary(xtable)
histo = R"function(x,...) hist(x,prob=TRUE,col='grey',border='white',...)"

dat = readdlm("../resources/etna.dat",',',header=true)
y_tmp = dat[1][1:size(dat[1])[1]-1,3]
# NOTE THAT THE LAST OBSERVATION IS MISSING!
n = length(y_tmp)
y = [Float64(x) for x in y_tmp]

R"par(mfrow=c(1,2))"
histo(y , xlab="", ylab="", main="Histogram of Interevent Times")
histo(log(y) , xlab="", ylab="", main="Histogram of Log Interevent Times")
R"par(mfrow=c(1,1))"

priors=Dict(:m=>0.0, :s2=>1.0, :a_tau=>1.0, :b_tau=>1.0,:a_sig=>1.0, :b_sig=>1.0)
@time post = auxGibbs.sample_t_hier(log(y), priors=priors)
post_smt = hcat(post[:sig2], post[:mu], post[:tau2])
plotposts(post_smt,tckdig=2,cnames=["σ²","μ","τ²"])

mu_vec_mean = mean(post[:mu_vec],1)'
mu_vec_hpd = hcat([hpd(post[:mu_vec][:,i]) for i in 1:n]...)'

R"dev.off()"
rng(x) = [minimum(x), maximum(x)]
plot(mu_vec_mean,xlab="",ylab="",ylim=rng(log(y)),pch=20,col="navy",cex=3)
points(log(y),xlab="",ylab="",col="grey60",pch=20,cex=2)
adderrbar(mu_vec_hpd,col="grey",lwd=2)
#plot(mu_vec_mean,log(y))
p_post_mu = PlotlyJS.scatter(x=collect(mu_vec_mean),mode=:markers,name="posterior draws:  μᵢ's",marker_size=10,marker_color=:cornflowerblue,
                             error_x=Dict("type"=>"data","array"=>randn(62),"visible"=>true))
p_log_y = PlotlyJS.scatter(x=log(y),mode=:markers,name="data: log(y)",marker_size=10,marker_color=:orange)
PlotlyJS.plot([p_log_y,p_post_mu],PlotlyJS.Layout(width=600,height=800,yaxis_zeroline=false))
#xtable()

PlotlyJS.plot(PlotlyJS.scatter(x=[1,2,3],y=[6,10,8],error_y=Dict("type"=>"data","array"=>[1,2,3],"visible"=>true)))
#https://plot.ly/julia/error-bars/
