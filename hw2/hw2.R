source("rwishert.R")
source("../R_Functions/plotPost.R",chdir=TRUE)

# Sufficient Stats:
Y <- as.matrix(iris[,1:2])
Y.bar <- apply(Y,2,mean)
c.rows <- t(apply(Y,1,function(y) y - Y.bar))
C.mat <- matrix(0,ncol(Y),ncol(Y))
for (i in 1:nrow(c.rows)) {
  C.mat <- C.mat + c.rows[i,] %*% t(c.rows[i,])
}

# Prior Params:
s <- 1
r <- 10
k <- length(Y.bar)
n <- nrow(Y)
S <- diag(k)
m <- Y.bar

# Posterior Params:
m.post <- (s*m + n*Y.bar) / (s+n) 
s.post <- s+n
r.post <- r+n
S.post <- S + C.mat + s*n/(s+n) * (Y.bar-m) %*% t(Y.bar-m)
iS.post <- solve(S.post) # IMPORTANT!!!

B <- 100000

postpred.y <- lapply(as.list(1:B),function(i) rniw.postpred(m.post,s.post,r.post,iS.post,brief=FALSE))

post.mu <- t(sapply(postpred.y,function(xx) xx$mu))
post.mu.info <- t(apply(post.mu,2,quantile,c(.025,.5,.975)))

post.S <- lapply(postpred.y, function(xx) xx$S)
post.S.info <- rbind(
quantile(sapply(post.S,function(i) i[1,1]),c(.025,.5,.975)),
quantile(sapply(post.S,function(i) i[2,2]),c(.025,.5,.975)),
quantile(sapply(post.S,function(i) i[1,2]),c(.025,.5,.975))
)

out.summary <- rbind(post.mu.info[1:2,],post.S.info)
out.summary
rownames(out.summary) <- c("Sepal Length", "Sepal Width", "SL Var", "SW Var", "SLW CoVar")
out.summary

pp <- t(sapply(postpred.y,function(xx) xx$postpred))
plot(Y[,1],Y[,2],xlab="Sepal Length",ylab="Sepal Width",pch=20,col="grey",cex=3)
post.mean <- apply(pp[,1:2],2,mean)
points(post.mean[1],post.mean[2],cex=5,col="cornflowerblue",pch=20)
plot.contour(pp[,1:2],add=TRUE,col="cornflowerblue")
