source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

Y <- dat[,-c(1:2)]
X <- log( Y / dat[,2] )

pairs(dat[,-1])
pairs(log(Y))
pairs(X)

plot.posts(log(Y),cex.a=1)
plot.posts(X,cex.a=1)

priors.list <- list("m"=apply(X,2,mean),"s"=1,"S"=diag(4),"r"=10)
postpred <- sample.niw(X,priors.list,B=10000)

post.mu <- t(sapply(postpred,function(xx) xx$mu))
post.S <- lapply(postpred,function(xx) xx$S)
post.pred <- t(sapply(postpred,function(xx) xx$postpred))

plot.posts(post.pred,cex.a=1,names=colnames(dat)[-c(1:2)])
plot.posts(post.mu,cex.a=1,names=colnames(dat)[-c(1:2)])
(post.S.mean <- func.matrices(post.S,mean))
(post.S.sd <- func.matrices(post.S,sd))

#31 -> Santa Cruz
plot.posts(exp(post.pred)*dat[31,2],cex.a=1,names=colnames(dat)[-c(1:2)])
plot.posts(exp(post.pred)*dat[31,2],cex.a=1,names=colnames(dat)[-c(1:2)],rng.x=c(0,.975))
plot.posts(exp(post.pred)*dat[1,2],cex.a=1,names=colnames(dat)[-c(1:2)],rng.x=c(0,.975))

plot(sort(X[,2]))
points(post.pred[order(X[,2]),2],col="grey",pch=20)

### HIERARCHICAL VERSION:
priors.hier <- list("m"=apply(X,2,mean),"v"=1,"S"=diag(4),"r"=10)
source("gibbs.R",chdir=TRUE)
out.hier <- gibbs.niw.hier(X,priors.hier,B=100)

out.hier$mu
apply(out.hier$mu.3d,1:2,mean)
apply(out.hier$S,1:2,mean)
