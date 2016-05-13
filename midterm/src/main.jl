# Arthur Lui's takehome midterm for AMS 207
# Review: BDA3 p. 294

using Distributions, RCall, DataFrames
import Bayes.hpd
include("auxGibbs.jl")
run(`mkdir -p ../report/figs`)

R"source('../../quiz/mypairs.R',chdir=TRUE)"
plotposts = R"simple.plot.posts"
plotpost = R"simple.plot.post"
adderrbar = R"add.errbar"
density = R"density"
@rlibrary(graphics)
#@rlibrary(xtable)
histo = R"function(x,color='grey',bordercol='white',...) 
            hist(x,prob=TRUE,col=color,border=bordercol,...)"
rgb(r,g,b,a=1) = rcopy(R"rgb"(r,g,b,a))

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

R"pdf('../report/figs/hist.pdf',h=4)"
R"par(mfrow=c(1,2))"
histo(y , xlab="", ylab="", main="Interevent Times")
histo(log(y) , xlab="", ylab="", main="Log Interevent Times")
R"par(mfrow=c(1,1))"
R"dev.off()"

priors=Dict(:m=>8.0, :s2=>100.0, :a_tau=>1.0, :b_tau=>1.0,:a_sig=>1.0, :b_sig=>1.0)
@time post = auxGibbs.sample_t_hier(log(y), priors=priors)
post_smt = hcat(post[:sig2], post[:mu], post[:tau2])

mu_vec_mean = mean(post[:mu_vec],1)'
mu_vec_hpd = hcat([hpd(post[:mu_vec][:,i]) for i in 1:n]...)'

postpred = auxGibbs.postpred_t_hier(post)
postpred_mean = mean(postpred,1)'
@time pp_hpd = hcat([hpd(postpred[:,i]) for i in 1:n]...)'

function prettyPlot(x,y,ci;xlab="Log Days",pch=20,colx="navy",coly="orange",
                    colhpd="grey",cex=2.5,main="",xlim=[minimum(y),maximum(y)+1],
                    label=years,title="",
                    legendx="PostPred")
  n = length(x)
  plot(x, 1:n, xlab=xlab,ylab="", xlim=xlim, pch=20,
       col=colx,cex=2.5,main="",bty="n",fg="grey",yaxt="n")
  R"title(main=$title, cex.main=2)"
  points(y,1:n, xlab="",ylab="",col=coly,pch=20,cex=1.5)
  adderrbar(ci,col=colhpd,lwd=2,trans=true)
  R"axis(2,label=$label,at=1:$n,las=2,cex.axis=.7,col='grey',col.axis='grey30')"
  R"legend('topright',legend=c($legendx,expression(log ~T[i]),'95% HPD'),
           col=c($colx,$coly,'red'),pch=20,cex=1.1,bg=rgb(.9,.8,.9,.8),
           box.col=rgb(.9,.8,.9,.8),pt.cex=2)"
end

@time post_simple = auxGibbs.sample_Normal(log(y),B=2000,burn=100000,
                      priors=Dict(:m=>8.0, :s2=>100.0, :a=>1.0, :b=>1.0))
pp_simple = auxGibbs.postpred_Normal(post_simple)

# Plot 1: Posterior for parameters ################
R"pdf('../report/figs/postparam.pdf')"
R"simple.plot.posts($post_smt,tckdig=2,cex.main=2,
                    cnames=c(expression(sigma^2),expression(mu),expression(tau^2)))"
R"dev.off()"
###################################################

# Plot 2: Posterior Predictives #############################################
R"pdf('../report/figs/postpred.pdf',w=5,h=12)"
prettyPlot(postpred_mean,log(y),pp_hpd,xlim=[minimum(pp_hpd),maximum(pp_hpd)],
           colhpd=rgb(1,0,0,.6))
R"dev.off()"
#############################################################################

# Plot 3: Posterior for parameters (SIMPLE MODEL)##
R"pdf('../report/figs/postparamsimple.pdf')"
post_simple_sm = [post_simple[:sig2] post_simple[:mu]]
R"simple.plot.posts($post_simple_sm,tckdig=2,cex.main=2,
                    cnames=c(expression(sigma^2),expression(mu)))"
R"dev.off()"
################################################### 

# Plot 4: Posterior Predictives (SIMPLE MODEL)#####
R"pdf('../report/figs/postpredsimple.pdf')"
plotpost(pp_simple,main="",trace=true)
histo(log(y),col=rgb(1,.65,0,.3),bordercol=rgb(1,1,1,.1),add=true)
R"legend('topleft',legend=c('Post. Predictive',expression(log~T[i])),bty='n',
         pch=15,col=c('cornflowerblue',rgb(1,.65,0)),cex=1.5,pt.cex=2)"
R"dev.off()"
################################################### 

dev_hier = auxGibbs.deviance_t_hier(post[:mu_vec],post[:sig2],log(y),5) # 189
dev_simp = auxGibbs.deviance_simple(post_simple[:mu],post_simple[:sig2],log(y)) # 206

