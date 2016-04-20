source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

pairs(dat[,-1])
pairs(log(dat[,-c(1:2)]/dat[,2]))
pairs(X)

Y <- dat[,-c(1:2)]
X <- log( Y / dat[,2] )

plot.posts(log(Y),cex.a=1)
plot.posts(X,cex.a=1)

priors.list <- list("m"=apply(X,2,mean),"s"=1,"S"=diag(4),"r"=10)
postpred <- sample.niw(X,priors.list,B=10000)

post.mu <- t(sapply(postpred,function(xx) xx$mu))
post.S <- lapply(postpred,function(xx) xx$S)
post.pred <- t(sapply(postpred,function(xx) xx$postpred))

plot.posts(post.pred,cex.a=1,names=colnames(dat)[-c(1:2)])
(post.S.mean <- func.matrices(post.S,mean))
(post.S.sd <- func.matrices(post.S,sd))

