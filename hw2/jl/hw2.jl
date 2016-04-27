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
plot_posts = R"plot.posts"
plot_posts(Y) 

for color in ["red", "green", "blue"]
    n = 750
    sims = randn(n,2)
    x = sims[:,1]; y = sims[:,2]
    scale = 200 * rand(n)
    scatter(x, y, c=color, s=scale, label=color,
            alpha=0.3, edgecolors="none")
end

legend()
PyPlot.xlabel("X")
PyPlot.ylabel("my Y")
grid(true)
