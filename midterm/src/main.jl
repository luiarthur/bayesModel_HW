# Arthur Lui's takehome midterm for AMS 207
# Review: BDA3 p. 294

using Distributions, RCall, DataFrames
include("auxGibbs.jl"); using auxGibbs

R"source('../../R_Functions/plotPost.R',chdir=TRUE)"
plotposts = R"plot.posts"
plotpost = R"plot.post"
density = R"density"
@rlibrary(graphics)
R"par(fg='grey')"
histo= R"function(x,...) hist(x,prob=TRUE,col='grey',border='white',...)"

dat = readdlm("../resources/etna.dat",',',header=true)
y_tmp = dat[1][1:size(dat[1])[1]-1,3]
# NOTE THAT THE LAST OBSERVATION IS MISSING!
n = length(y_tmp)
y = [Float64(x) for x in y_tmp]

R"par(mfrow=c(1,2))"
histo(y , xlab="", ylab="", main="Histogram of Interevent Times")
histo(log(y) , xlab="", ylab="", main="Histogram of Log Interevent Times")
R"par(mfrow=c(1,1))"

include("auxGibbs.jl")
@time post = auxGibbs.sample_t_hier(log(y))
post_smt = hcat(post[:sig2], post[:mu], post[:tau2])
plotposts(post_smt,names=["σ²","μ","τ²"])

R"source('../../quiz/mypairs.R',chdir=TRUE)"
mypairs = R"my.pairs"
splotposts = R"simple.plot.posts"
mypairs(post_smt)
splotposts(post_smt,tckdig=1,names=["σ²","μ","τ²"])
