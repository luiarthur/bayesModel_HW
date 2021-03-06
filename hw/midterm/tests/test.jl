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

#R"myqqplot <- function(x,y,...) {
#  qx <- quantile(x,1:100/100)
#  qy <- quantile(y,1:100/100)
#  plot(qx,qy,...)
#}"
#R"myqqplot(log($y),$postpred_mean)"
#histo(log(y),ylim=[0,1])
#R"lines(density($postpred_mean))"



#include("../src/auxGibbs.jl")
#using auxGibbs, RCall
#?auxGibbs.sample_Normal
#x = auxGibbs.sample_Normal(randn(1000)+3,B=2000,burn=100000)
#mean(x[:mu])
#mean(x[:sig2])
#pp = auxGibbs.postpred_Normal(x)
#R"curve(dnorm(x,3,1),col='red',lwd=2,from=0,to=6)"
#R"hist($pp,prob=TRUE,add=TRUE)"


using RCall
R"source('../../quiz/mypairs.R',chdir=TRUE)"
plotpost = R"simple.plot.post"
plotpost(randn(1009),trace=true)
