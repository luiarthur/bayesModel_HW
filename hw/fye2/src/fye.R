set.seed(1)
source("gibbs.R")
source("../../quiz/mypairs.R",chdir=TRUE)
dat <- read.csv("../dat/fye.dat",header=TRUE)
rig <- function(n,shape,rate) 1/rgamma(n,shape=shape,rate=rate)

# (a)
cMR <- dat[,"ctrlMortRate"]/100
tMR <- dat[,"trtMortRate"]/100
cN <- dat[,"ctrlN"]
tN <- dat[,"trtN"]
y <- cMR - tMR
k <- nrow(dat) # number of studies

t(apply(rbind(cMR, tMR, y),1,summary))
a_out <- t(rbind(cMR,tMR,y))
colnames(a_out) <- c("Ctrl_MR","Trt_MR","y")
rownames(a_out) <- dat$study

plotTCY <- function() {
  plot(a_out[,1]*100,pch=16,type='o',cex=1.5,lwd=3,col="red",
       ylim=range(a_out*100),fg='grey',bty='n',
       ylab="Mortality Rate (%)", xlab="Study",
       xaxt="n")
  abline(h=0,col="grey",lwd=1)
  lines(a_out[,2]*100,pch=16,type='o',cex=1.5,lwd=3,col="blue")
  lines(a_out[,3]*100,pch=16,type='o',cex=1.5,lwd=3,col="darkgreen")
  axis(1,fg='grey',at=1:k,labels=dat$study)
  abline(v=1:k,col="grey",lty=3)
  legend("topleft",legend=c("Control","Treatment","y=(C-T)"),
         text.col=c("red","blue","darkgreen"),bty="n",text.font=2,cex=2)
}

pdf("../tex/img/tcy.pdf")
plotTCY()
dev.off()

### M1
V <- cMR*(1-cMR)/cN + tMR*(1-tMR)/tN
stopifnot(length(V) == length(y) && length(y) == 6)

mu.post.mean.M1 <- sum(y/V) / sum(1/V)
mu.post.var.M1 <- 1/sum(1/V)
#simple.plot.post(mu.post.M1.samps)

f.post <- function(x) dnorm(x,mu.post.mean.M1,sd=sqrt(mu.post.var.M1))
r.post <- function(n) rnorm(n,mu.post.mean.M1,sd=sqrt(mu.post.var.M1))
q.post <- function(p) qnorm(p,mu.post.mean.M1,sd=sqrt(mu.post.var.M1))
F.post <- function(x) pnorm(x,mu.post.mean.M1,sd=sqrt(mu.post.var.M1))

M1.plot.post <- function() {
  curve(f.post,from=q.post(1e-5), to=q.post(1-1e-5), cex.lab=1.3,
        col='white',bty="n",fg="grey", ylab="Density",xlab=expression(mu))
  color.fn(f.post,from=q.post(1e-5), to=q.post(1-1e-5),
           col=col.mult('dodgerblue','darkgrey'))
  color.fn(f.post,from=0, to=1,col.mult('navy'))
  abline(v=c(mu.post.mean.M1),col=c('red'),lwd=2)
  legend("topleft",bty="n",cex=1,text.col="grey30",
        legend=paste(c("Mean =", "SD =",
                       "HPD Lower =", "HPD Upper =",
                       "P(mu > 0 | y, M1) ="),
                      round(c(mu.post.mean.M1,sqrt(mu.post.var.M1),
                              q.post(c(.025,.975)), 1-F.post(0)),4)))
}

pdf("../tex/img/m1Post.pdf")
M1.plot.post()
dev.off()

post.pred.M1 <- t(sapply(1:10000,function(x) rnorm(6,r.post(1),V)))
apply(post.pred.M1,2,mean)

q.post(c(.025,.975))


### M2
# rig(n,a,b) <- 1/rgamma(n,a,b)
# IG(3,1) => mean=.5, sd=.5
source("gibbs.R")
# This is when M2 -> M1
#out <- gibbs(y=y,V=V,B=10000,burn=50000,params=list(siga=100,sigb=.001))
# This understates the certainty of mu
#out <- gibbs(y=y,V=V,B=10000,burn=5000,params=list(siga=3,sigb=1))
# This also, this has variance of infty
#out <- gibbs(y=y,V=V,B=10000,burn=5000,params=list(siga=1,sigb=1))
# This maybe: because variance can't go above .16
out <- gibbs(y=y,V=V,B=10000,burn=5000,params=list(siga=3,sigb=.3))

pdf("../tex/img/m2MuS2Post.pdf")
plot.posts(out[,1:2],names=colnames(out)[1:2],cex.a=1,rng.x=c(0,.999))
dev.off()

pdf("../tex/img/thetaPost.pdf")
simple.plot.posts(out[,-c(1:2)],cex.a=.7)
dev.off()

mu.post <- out[,"mu"]

pdf("../tex/img/m2MuPost.pdf")
  simple.plot.post(mu.post,trace=TRUE,main="",xlab=expression(mu),cex.lab=1.3)
  color.den(density(mu.post),from=-1,to=1,col.a=col.mult("navy"),
            col.d='white',add=TRUE)
  color.den(density(mu.post),from=-1,to=0,col.a="dodgerblue",
            col.d='transparent',add=TRUE)
  lines(density(mu.post),col="white",lwd=3)
  legend("topleft",bty="n",cex=1,text.col="grey30",
        legend=paste(c("Mean =", "SD =",
                       "HPD Lower =", "HPD Upper =",
                       "P(mu > 0 | y, M1) ="),
                      round(c(mean(mu.post),sd(mu.post),
                              quantile(mu.post,c(.025,.975)),
                              mean(mu.post>0) ),4)))
  abline(v=mean(mu.post),col='red',lwd=2)
dev.off()
#(M2.mu.p <- mean(mu.post > 0)) # .572
# mean(rep(cMR - tMR,(cN+tN)) > 0) # .5817 # THIS IS WHAT I SHOULD EXPECT!!!

apply(out[,-c(1:2)],2,mean)

#########
#x <- rig(1000000,100,.001); mean(x); sd(x)
#x <- rig(1000000,3,1); mean(x); sd(x)
#x <- rig(1000000,3,.3); mean(x); sd(x)

#sd(sapply(1:10000,function(x) sd(runif(5,-1,1))))

### DIC
DIC.M1 <- function(post.samps) {
  d <- function(mu) {
    stopifnot(length(y) == length(V))
    -2 * sum(dnorm(y,mu,sqrt(V),log=TRUE))
  }
  ds <- sapply(post.samps, d)
  print(mean(ds))
  print(var(ds))
  mean(ds) + var(ds)/2
}

DIC.M2 <- function(post.th) {
  stopifnot(ncol(post.th) == length(y))
  d <- function(th) {
    stopifnot(length(y) == length(V))
    stopifnot(length(y) == length(th))
    -2 * sum(dnorm(y,th,sqrt(V),log=TRUE))
  }
  ds <- apply(post.th,1, d)
  print(mean(ds))
  print(var(ds))
  mean(ds) + var(ds)/2
}

dic.m1 <- DIC.M1(r.post(10000))
dic.m2 <- DIC.M2(out[,-c(1:2)])

dic.m2 - dic.m1
