# Arthur Lui's takehome midterm for AMS 207
# Review: BDA3 p. 294

using Distributions, RCall, DataFrames
import Bayes.hpd
include("auxGibbs.jl")

R"source('../../quiz/mypairs.R',chdir=TRUE)"
plotposts = R"simple.plot.posts"
plotpost = R"simple.plot.post"
adderrbar = R"add.errbar"
density = R"density"
@rlibrary(graphics)
@rlibrary(xtable)
histo = R"function(x,...) hist(x,prob=TRUE,col='grey',border='white',...)"
rgb(r,b,g,a=1) = rcopy(R"rgb"(r,g,b,a))

dat = readdlm("../resources/etna.dat",',',header=true)
y_tmp = dat[1][1:size(dat[1])[1]-1,3]
# NOTE THAT THE LAST OBSERVATION IS MISSING!
n = length(y_tmp)
y = [Float64(x) for x in y_tmp]
#years = [ASCIIString(replace(dat[1][i,2]," ","-" )) for i in 1:n]
mo_year = [ string(dat[1][i,2][6:7],"   ",dat[1][i,2][1:4]) for i in 1:n]
mo_yr = Dict("01" => "Jan", "02" => "Feb", "03" => "Mar", 
             "04" => "Apr", "05" => "May", "06" => "Jun",
             "07" => "Jul", "08" => "Aug", "09" => "Sep",
             "10" => "Oct", "11" => "Nov", "12" => "Dec")
years = [ string(mo_yr[yr[1:2]]," ", yr[6:9]) for yr in mo_year]

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

rng(x) = [minimum(x), maximum(x)+1]

plot(mu_vec_mean, 1:n, xlab="Log Days",ylab="", xlim=rng(log(y)), pch=20,
     col="navy",cex=2.5,main="",bty="n",fg="grey",yaxt="n")
R"title(main='Interevent Times', cex.main=2)"
points(log(y),1:n, xlab="",ylab="",col="orange",pch=20,cex=1.5)
adderrbar(mu_vec_hpd,col="grey",lwd=3,trans=true)
R"axis(2,label=$years,at=1:$n,las=2,cex.axis=.7,col='grey',col.axis='grey30')"
R"legend('topright',legend=c('Post Pred',expression(log ~T[i])),
         col=c('navy','orange'),pch=20,cex=1.1,bg=rgb(.9,.8,.9,.5),box.lwd=0)"
abline(h=collect(round( linspace(1,n,20) )),col="grey80",lwd=.5)

R"myqqplot <- function(x,y,...) {
  qx <- quantile(x,1:100/100)
  qy <- quantile(y,1:100/100)
  plot(qx,qy,...)
}"
R"myqqplot(log($y),$postpred_mean)"
histo(log(y),ylim=[0,1])
R"lines(density($postpred_mean))"

# PlotlyJS
# using PlotlyJS, Rsvg
#p_post_mu = scatter(x=collect(mu_vec_mean),y=years,
#                    mode=:markers,name="posterior draws: local mu_i",
#                    marker_size=10,marker_color=:cornflowerblue)
#p_log_y = scatter(x=log(y),y=years,mode=:markers,name="data: log T_i",
#                  marker_size=10,marker_color=:orange)
#lines = [scatter(y=fill(years[i],2), x=collect(mu_vec_hpd[i,:]), 
#                 mode=:lines, showlegend=false, line_color=:grey) 
#                 for i in 1:n]
#plotdata = Base.typed_vcat(PlotlyJS.GenericTrace, p_post_mu, p_log_y, lines)
##https://plot.ly/julia/error-bars/
#interevent = PlotlyJS.plot(plotdata,Layout(title="Interevent Times",width=600,height=800,
#                           legend=attr(x=0,y=1,bgcolor=rgb(.5,.5,.5,.5)),
#                           yaxis=attr(zeroline=false),yaxis_type=:category,margin_l=80))
#interevent
#PlotlyJS.savefig3(interevent,"tmp.pdf")
