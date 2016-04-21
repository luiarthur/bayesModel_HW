source("mypairs.R")
source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
source("plotmap.R")
library(maps)

# READ DATA: ######################################
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

Y <- dat[,-c(1:2)]
X <- log( Y / dat[,2] ) # log(crimes per capita)

# VISUALIZE: ##############################################################
my.pairs(log(dat[,-1])) # Obviously, if number of one crime is high, 
                        # the number of any other crime will be high as well
                        # because there is a confounding with popluation.
my.pairs(X)             # A better measure of crime is crime per capita

county <- paste(dat[,1])
county[14] <- "Marin"
state <- "California"

col.pal <- colorRampPalette(c("chartreuse3","pink","brown1"))(5)
plot.per.county(log(dat[,2]),state,county,m="log Population",col=col.pal)
dev.off()

par(mfrow=c(1,4))
plot.per.county(dat[,3]/dat[,2]*100000,state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Robbery\t\t\t")
plot.per.county(dat[,4]/dat[,2]*100000,state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Burglary\t\t\t")
plot.per.county(dat[,5]/dat[,2]*100000,state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Larceny\t\t\t")
plot.per.county(dat[,6]/dat[,2]*100000,state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Vehicle\t\t\t")
par(mfrow=c(1,1))
title("Crimes per 100000",cex.main=4,line=-2)

# Modeling log(Crime / Capita):  ################################
priors.list <- list("m"=apply(X,2,mean),"s"=1,"S"=diag(4),"r"=10)
postpred <- sample.niw(X,priors.list,B=100000,print=TRUE)
postpred <- tail(postpred,10000)

post.mu <- t(sapply(postpred,function(xx) xx$mu))
post.S <- lapply(postpred,function(xx) xx$S)
post.pred <- t(sapply(postpred,function(xx) xx$postpred))

plot.posts(post.pred,cex.a=1,names=colnames(dat)[-c(1:2)])
plot.posts(post.mu,cex.a=1,names=colnames(dat)[-c(1:2)])
(post.S.mean <- func.matrices(post.S,mean))
(post.S.sd <- func.matrices(post.S,sd))

#31 -> Santa Cruz
plot.posts(exp(post.pred)*dat[31,2],cex.a=1,names=colnames(dat)[-c(1:2)],rng.x=c(0,.975))
dev.off()

plot.per.county(post.pred[,4],state,county,m="postpred",col=col.pal)
plot.per.county(X[,4],state,county,dig=5,col=col.pal,m="Data")
