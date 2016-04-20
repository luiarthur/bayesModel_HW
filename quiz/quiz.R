source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

Y <- dat[,-c(1:2)]
X <- log( Y / dat[,2] )

pairs(dat[,-1])
pairs(log(Y))
pairs(X)
pairs(log(dat[,-1]))

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
out.hier <- gibbs.niw.hier(X,priors.hier,B=1000)

out.hier$mu
apply(out.hier$mu.3d,1:2,mean)
apply(out.hier$S,1:2,mean)

l <- 1
plot(sort(X[,l]))
points(apply(out.hier$mu.3d,1:2,mean)[order(X[,l]),l],pch=20,col="blue")

library(maps)
state.county <- paste0('california,',dat[,1])
state.county[14] <- "california,Marin"
state.county <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', state.county)

map('county','california',col="grey90")
map('county',state.county,names=TRUE,col="red",add=TRUE,fill=TRUE,border="grey90")
for (i in 1:length(state.county)) {
  rng <- map('county',state.county[i],plot=FALSE)$range
  text((rng[1]+rng[2])/2, (rng[3]+rng[4])/2,dat[i,1],cex=.5)
  Sys.sleep(1)
}
cnty.info <- map('county',state.county,plot=FALSE)
cnty.info$y


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
