set.seed(207)
library(Rcpp)
source('../../quiz/mypairs.R',chdir=TRUE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11") # enable c++11, for RcppArmadillo
sourceCpp("C++/blasso.cpp")

histo <- function(x,color='grey',bordercol='white',...) 
           hist(x,prob=TRUE,col=color,border=bordercol,...)
dat <- read.csv('../dat/poll.dat',header=TRUE,sep='\t')
n <- nrow(dat)
print( head(dat) )
leave <- dat[,"LeavePerc"]
stay <- dat[,"RemainPerc"]
date <- sapply(dat[,"Date"],as.character)
date_ind <- n:1
src <- sapply(dat[,"Source"],as.character)
meth <- sapply(dat[,"Method"],as.character)
X.mod <- model.matrix(~date_ind+src+meth)
X <- as.matrix(X.mod)
X_colnames <- colnames(X)
logit <- function(p) log(p/(1-p))
invlogit <- function(w) 1/(1+exp(-w))
p <- leave / (stay+leave)
y <- logit(p)
dat_short <- cbind(y[-1],y[-n],dat[-1,-1])
colnames(dat_short)[1:2] <- c("logit_p","logit_p_lag1")
y_short <- dat_short[,1]
X_short <- model.matrix(~logit_p_lag1+Source+Method,data=dat_short)

# Plots
pairs(dat,col="orange",pch=20,bty='n',fg='black')
par(bty="o",fg="grey40",col.axis="grey30")
#pairs(cbind(y,dat[,-c(3:5)]),pch=20,col='orange')
pairs(dat_short[,-c(3:5)],pch=20,col='orange')
summary( lm(logit_p~logit_p_lag1+Source+Method,data=dat_short)  )

sy_short <- scale(y_short)
sX_short <- scale(X_short[,-1])

system.time( blasso.mod <- blasso(y=sy_short,x=sX_short, r=1,
                                  delta=1.5,B=20000, burn=10000, 
                                  printProg=F, returnHyper=TRUE)) 

my.pairs(blasso.mod$beta[1:1000,1:10])
colnames(blasso.mod$beta) <- colnames(sX_short)
simple.plot.posts(blasso.mod$beta[1:1000,1:5])
simple.plot.posts(blasso.mod$beta[1:1000,6:10])

post.mean <- apply(blasso.mod$beta,2,mean)
hpd <- apply(blasso.mod$beta,2,get.hpd)

k <- ncol(blasso.mod$beta)
par(mar=c(5,8,1,1))
plot(post.mean,1:k,pch=20,col="dodgerblue",cex=3,xlim=c(-1,1),yaxt="n",bty="n",
     ylab="",fg="grey")
axis(2,at=1:k,label=colnames(sX_short),las=2,fg="grey")
add.errbar(t(hpd),trans=TRUE,col="dodgerblue")
abline(v=0,col="grey")
par(mar=c(5,4,4,2)+.1)

back_trans <- function(b,s=names(b)) {
  invlogit( b*sd(X_short[,s]) * sd(y_short) + mean(y_short) )
}
back_trans(post.mean["Methodphone"],"Methodphone")
back_trans(post.mean["SourceIpsos Mori"])

#blasso.mod
