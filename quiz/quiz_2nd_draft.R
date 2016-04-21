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
title("Thefts per 100000",cex.main=4,line=-2)

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


# Postpred. Expected Number of Each Theft in Each County.
pm.crime <- dat[,2] %*% matrix(apply(post.pred,2,function(x) mean(exp(x))),1)
par(mfrow=c(2,4))
plot.per.county(pm.crime[,1],state,county,dig=0,paren=F,bks=c(0,15,50,100,300,600),col=col.pal,m="Posterior Predictive - Robbery")
plot.per.county(pm.crime[,2],state,county,dig=0,paren=F,bks=c(0,15,50,100,300,600),col=col.pal,m="Posterior Predictive - Burglary")
plot.per.county(pm.crime[,3],state,county,dig=0,paren=F,bks=c(0,15,50,100,300,600),col=col.pal,m="Posterior Predictive - Larceny")
plot.per.county(pm.crime[,4],state,county,dig=0,paren=F,bks=c(0,15,50,100,300,600),col=col.pal,m="Posterior Predictive - Vehicle")
plot.per.county(Y[,1],state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Data - Robbery\t\t\t")
plot.per.county(Y[,2],state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Data - Burglary\t\t\t")
plot.per.county(Y[,3],state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Data - Larceny\t\t\t")
plot.per.county(Y[,4],state,county,dig=0,bks=c(0,15,50,100,300,600),col=col.pal,paren=F,m="Data - Vehicle\t\t\t")
par(mfrow=c(1,1))

# Postpred. Expected Total Number of Thefts in each county.
par(mfrow=c(1,2))
plot.per.county(apply(pm.crime,1,sum),state,county,dig=0,paren=F,bks=seq(0,4000,len=6),col=col.pal,m="Posterior Predictive - Thefts")
plot.per.county(apply(Y,1,sum),state,county,dig=0,bks=seq(0,4000,len=6),col=col.pal,paren=F,m="Data - Thefts\t\t\t")
par(mfrow=c(1,1))


# Santa Cruz Thefts
pp.sc <- apply(post.pred,2,function(x) exp(x) * dat[31,2])
plot.posts(pp.sc,rng.x=c(0,.999),cex.a=1,cex.l=1,names=colnames(dat[,3:6]))

pp.sum.sc <- apply(pp.sc,1,sum)
plot.post(pp.sum.sc,stay=TRUE)

my.color(density(pp.sum.sc),from=-5,to=3000,lwd=6,bty="n",main="",
         col.area="cornflowerblue",col.den="cornflowerblue",fg="grey")
my.color(density(pp.sum.sc),from=3000,to=50000,col.area="blue",add=TRUE,col.den="blue",lwd=6)
(prob.sc.above.3000 <- mean(pp.sum.sc > 3000))
text(15000,1e-04, paste0("Prob. Santa Cruz Thefts > 3000 is ",prob.sc.above.3000*100,"%"),cex=1)
