source("mypairs.R")
source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
source("plotmap.R")
library(maps)

# READ DATA: ######################################
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

Y <- dat[,-c(1:2)]
X <- log( Y / dat[,2] )

# VISUALIZE: ##############################################################
my.pairs(log(dat[,-1])) # Obviously, if number of one crime is high, 
                        # the number of any other crime will be high as well
                        # because there is a confounding with popluation.
my.pairs(X)             # A better measure of crime is crime rates.

county <- paste(dat[,1])
county[14] <- "Marin"
state <- "California"

col.pal <- colorRampPalette(c("chartreuse3","pink","brown1"))(5)
plot.per.county(log(dat[,2]),state,county,m="log Population",col=col.pal)

source("plotmap.R")
plot.per.county(dat[,3]/dat[,2]*100,state,county,dig=3,col=col.pal,m="Robb",per=T)
plot.per.county(dat[,4]/dat[,2]*100,state,county,dig=3,col=col.pal,m="Burg",per=T)
plot.per.county(dat[,5]/dat[,2]*100,state,county,dig=3,col=col.pal,m="Larc",per=T)
plot.per.county(dat[,6]/dat[,2]*100,state,county,dig=3,col=col.pal,m="Vehi",per=T)

plot.per.county(log(dat[,6]/dat[,2]),state,county,dig=3,col=col.pal,m="Vehi")
############################################################################


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
plot.posts(exp(post.pred)*dat[31,2],cex.a=1,names=colnames(dat)[-c(1:2)],rng.x=c(0,.975))
plot.posts(exp(post.pred)*dat[1,2],cex.a=1,names=colnames(dat)[-c(1:2)],rng.x=c(0,.975))

plot(sort(X[,4]))
points(post.pred[order(X[,4]),4],col="grey",pch=20)
plot.per.county(post.pred[,4],state,county,m="postpred",col=col.pal)
X11();
plot.per.county(X[,4],state,county,dig=5,col=col.pal,m="Data")

### HIERARCHICAL VERSION:
priors.hier <- list("m"=apply(X,2,mean),"v"=1,"S"=diag(4),"r"=10)
out.hier <- gibbs.niw.hier(X,priors.hier,B=100)

out.hier$mu
apply(out.hier$mu.3d,1:2,mean)
apply(out.hier$S,1:2,mean)

l <- 1
plot(sort(X[,l]))
points(apply(out.hier$mu.3d,1:2,mean)[order(X[,l]),l],pch=20,col="blue")

# HIER on log(Y)
priors.hier.2 <- list("m"=apply(log(Y),2,mean),"v"=1,"S"=diag(4),"r"=10)
out.hier.2 <- gibbs.niw.hier(log(Y),priors.hier.2,B=100)
mu.2 <- apply(out.hier.2$mu.3d,1:2,mean)

source("plotmap.R")
par(mfrow=c(1,2))
plot.per.county(log(dat[,6]),state,county,m="log num of Stolen Vehicles",col=col.pal)
plot.per.county(mu.2[,4],state,county,"H-post",col=col.pal,bks=c(0,1.8,2.7,3.7,5.4,10))
par(mfrow=c(1,1))




# Regression with unknown covariance matrix (p.370)

# This is how you get the fips
#data(county.fips)
#dat.fips <- county.fips[which(paste(county.fips[,2]) %in% state.county),1]
#
#dat.fips_and_county <- cbind(dat.fips,state.county)


#cbind(dat[,2],apply(exp(out.hier$mu.3d) * dat[,2],1:2,mean)-Y)
#apply(exp(out.hier$mu.3d) * dat[,2],1:2,sd)
#
#exp(apply(out.hier$mu.3d,1:2,mean)) * dat[,2]
#exp(apply(out.hier$mu.3d,1:2,sd)) * dat[,2]
