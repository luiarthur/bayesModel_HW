source("gibbs.R")
source("../../quiz/mypairs.R",chdir=TRUE)
dat <- read.csv("../dat/fye.dat",header=TRUE)

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
  curve(f.post,from=q.post(1e-5), to=q.post(1-1e-5), cex.lab=1.5,
        col='white',bty="n",fg="grey", ylab="density",xlab=expression(mu))
  color.fn(f.post,from=q.post(1e-5), to=q.post(1-1e-5),
           col=col.mult('dodgerblue','darkgrey'))
  color.fn(f.post,from=q.post(.025), to=q.post(.975),col.mult('navy'))
  abline(v=c(mu.post.mean.M1,0),col=c('red','grey'),lwd=2)
  legend("topright",bty="n",cex=1,text.col="grey30",
        legend=paste(c("Mean =", "SD =","P(mu > 0 | y, M1) ="),
                      round(c(mu.post.mean.M1,sqrt(mu.post.var.M1),
                              1-F.post(0)),4)))
}

pdf("../tex/img/m1Post.pdf")
M1.plot.post()
dev.off()

q.post(c(.025,.975))

#rig(n,a,b) <- 1/rgamma(n,a,b)
# IG(3,1) => mean=.5, sd=.5

out <- gibbs(y=y,V=V,B=5000,burn=5000)
plot.posts(out[,1:2],names=colnames(out)[1:2])
simple.plot.posts(out[,-c(1:2)])
mu.post <- out[,"mu"]
(M2.mu.p <- mean(mu.post > 0))
