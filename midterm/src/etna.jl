using Distributions, RCall, DataFrames
R"source('../../R_Functions/plotPost.R',chdir=TRUE)"
plotposts = R"plot.posts"
@rlibrary(graphics)

dat = readdlm("../resources/etna.dat",',',header=true)
y = dat[1][:,3]
n = length(y)

plot([Float64(x) for x in y[1:n-1]], xlab="", ylab="")
