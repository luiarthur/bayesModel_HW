println("Loading Libraries...")
using RCall, Distributions, PyPlot

R"
source('../../R_Functions/plotPost.R',chdir=TRUE)

Y <- as.matrix(iris[,1:2])
Y_bar <- apply(Y,2,mean)
c_rows <- t(apply(Y,1,function(y) y - Y_bar))
C_mat <- matrix(0,ncol(Y),ncol(Y))
for (i in 1:nrow(c_rows)) {
  C_mat <- C_mat + c_rows[i,] %*% t(c_rows[i,])
}

# Prior Params:
s <- 1
r <- 10
k <- length(Y_bar)
n <- nrow(Y)
S <- diag(k)
m <- Y_bar

# Posterior Params:
m_post <- (s*m + n*Y_bar) / (s+n) 
s_post <- s+n
r_post <- r+n
iS_post <- solve(S) + C_mat + s*n/(s+n) * (Y_bar-m) %*% t(Y_bar-m)
S_post <- solve(iS_post) # IMPORTANT!!!

B <- 100000
"
@rget Y Y_bar C_mat m_post s_post r_post iS_post S_post B
var_names = ["Sepal Length", "Sepal Width"]
plot_posts = R"plot.posts"

function rniw(m,s,r,iS,brief=true)
  iW = rand(InverseWishart(r,iS))
  mu = rand(MultivariateNormal(m,iW/s))
  postpred = rand(MultivariateNormal(mu,iW))
  out =  brief ? postpred : (mu,iW,postpred)
  out
end

par = m_post, s_post, r_post, iS_post, false
@time par_list = [par for i in 1:B]
println("Sampling...")
@time out = map(x -> rniw(x[1],x[2],x[3],x[4],x[5]), par_list);

post_mu = map(x -> float(x), 
              hcat(map(x -> x[1][1], out), 
                   map(x -> x[1][2], out)))
#=
include("hw2.jl")
plot_posts(Y,names=var_names) 
plot_posts(post_mu[B*.95:end,:],names=var_names)
=#
