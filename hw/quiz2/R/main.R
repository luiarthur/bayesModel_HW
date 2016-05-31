set.seed(207)
library(Rcpp)
source('../../quiz/mypairs.R',chdir=TRUE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11") # enable c++11, for RcppArmadillo
print("Loading blasso.cpp")
sourceCpp("C++/blasso.cpp")
print("Done.")
system("mkdir -p ../report/figs")

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
#pairs(dat,col="orange",pch=20,bty='n',fg='black')
#pairs(cbind(y,dat[,-c(3:5)]),pch=20,col='orange')
pdf("../report/figs/pairs.pdf")
par(bty="o",fg="grey40",col.axis="grey30")
pairs(dat_short[,-c(3:5)],pch=20,col='orange')
dev.off()
summary( lm(logit_p~logit_p_lag1+Source+Method,data=dat_short)  )

sy_short <- scale(y_short)
sX_short <- scale(X_short[,-1])

lvout <- sample(1:(length(sy_short)),50)
system.time( blasso.mod <- blasso(y=sy_short[-lvout],x=sX_short[-lvout,], r=1,
                                  delta=1.5, burn=100000, B=102000,
                                  printProg=TRUE, returnHyper=TRUE)) 

#my.pairs(blasso.mod$beta[1:1000,1:10])
colnames(blasso.mod$beta) <- colnames(sX_short)
#simple.plot.posts(blasso.mod$beta[1:1000,1:5])
#simple.plot.posts(blasso.mod$beta[1:1000,6:10])
pdf("../report/figs/posts.pdf")
simple.plot.posts(blasso.mod$beta[,c(4,10)],ma=3,col.main="grey30",
                  cnames=c("Source -- Ipsos Mori", "Method -- Phone"))
dev.off()

post.mean <- apply(blasso.mod$beta,2,mean)
hpd <- apply(blasso.mod$beta,2,function(x) get.hpd(x,a=.05,len=1e3))


k <- ncol(blasso.mod$beta)
pdf("../report/figs/allposts.pdf")
par(mar=c(5,8,1,1))
plot(post.mean,1:k,pch=20,col="dodgerblue",cex=3,xlim=c(-1.1,1.1),yaxt="n",bty="n",
     ylab="",fg="grey",xlab="Posterior Mean ")
axis(2,at=1:k,label=colnames(sX_short),las=2,fg="grey")
add.errbar(t(hpd),trans=TRUE,col="dodgerblue")
abline(v=0,col="grey")
par(mar=c(5,4,4,2)+.1)
dev.off()

# Plot the probability for a few interesting particular cases.
# MethodPhone = 1, all else mean
mux <- attr(scale(X_short[-lvout,-1]),"scaled:center")
sdx <- attr(scale(X_short[-lvout,-1]),"scaled:scale")
muy <- mean(y_short[-lvout])
sdy <- sd(y_short[-lvout])

pred <- function(x,gam) {
  invlogit( sdy * sum(-mux*gam/sdx + x*gam/sdx) + muy )
}

x00 <- c(mux[1:3],0,mux[5:9],0)
x01 <- c(mux[1:3],0,mux[5:9],1)
x10 <- c(mux[1:3],1,mux[5:9],0)
x11 <- c(mux[1:3],1,mux[5:9],1)
pred00 <- pred(x00,post.mean)
pred01 <- pred(x01,post.mean)
pred10 <- pred(x10,post.mean)
pred11 <- pred(x11,post.mean)
#pdf("../report/figs/preds.pdf")
#plot(c(0,1),c(pred00,pred01),bty="n",fg="grey",ylim=c(.4,.5),type='l',
#     col="grey",xlab="",ylab="Proportion in Favor of  Leaving")
#lines(c(0,1),c(pred10,pred11),col="grey")
#points(c(0,1,0,1),c(pred00,pred01,pred10,pred11),pch=20,cex=4,bty="n",
#       fg="grey",ylim=c(.4,.5), col=c("red","purple","black","orange"),
#       xlab="",ylab="Proportion in Favor of  Leaving")
#legend("topright", col=c("red","purple","black","orange"),pch=20,pt.cex=2,
#       legend=c("BGM Mori, Online","BGM, Phone","Ipsos Mori, Online",
#                "Ipsos Mori, Phone"),bty="n")
#dev.off()

# mori,phone
ph_mo   <- which(X_short[lvout,"Methodphone"]==1 & X_short[lvout,"SourceIpsos Mori"]==1)
ph_Nmo  <- which(X_short[lvout,"Methodphone"]==1 & X_short[lvout,"SourceIpsos Mori"]==0)
Nph_mo  <- which(X_short[lvout,"Methodphone"]==0 & X_short[lvout,"SourceIpsos Mori"]==1)
Nph_Nmo <- which(X_short[lvout,"Methodphone"]==0 & X_short[lvout,"SourceIpsos Mori"]==0)

X_lvout_ph_mo <- sX_short[lvout,][ ph_mo,]
X_lvout_ph_Nmo <- sX_short[lvout,][ ph_Nmo,]
X_lvout_Nph_mo <- sX_short[lvout,][Nph_mo,]
X_lvout_Nph_Nmo <- sX_short[lvout,][Nph_Nmo,]

y_lvout <- sy_short[lvout]
y_lvout_ph_mo <- y_lvout[ph_mo]
y_lvout_ph_Nmo <- y_lvout[ph_Nmo]
y_lvout_Nph_mo <- y_lvout[Nph_mo]
y_lvout_Nph_Nmo <- y_lvout[Nph_Nmo]

p_lvout_ph_mo <- invlogit(y_lvout_ph_mo)
p_lvout_ph_Nmo <- invlogit(y_lvout_ph_Nmo)
p_lvout_Nph_mo <- invlogit(y_lvout_Nph_mo)
p_lvout_Nph_Nmo <- invlogit(y_lvout_Nph_Nmo)

predx_ph_mo   <- apply(X_lvout_ph_mo,1,function(x) pred(x,post.mean))
predx_ph_Nmo  <- apply(X_lvout_ph_Nmo,1,function(x) pred(x,post.mean))
predx_Nph_mo  <- apply(X_lvout_Nph_mo,1,function(x) pred(x,post.mean))
predx_Nph_Nmo <- apply(X_lvout_Nph_Nmo,1,function(x) pred(x,post.mean))

pdf("../report/figs/preds.pdf")
xlim <- range(invlogit(X_short[lvout,"logit_p_lag1"]))
ylim <- range(predx_ph_mo,predx_ph_Nmo,predx_Nph_mo,predx_Nph_Nmo,invlogit(y_lvout),1)
# Data:
plot(  invlogit(X_short[lvout,"logit_p_lag1"][ph_mo]),
       invlogit(y_lvout)[ph_mo],
       col="red",pch=20,cex=3,xlim=xlim,ylim=ylim,
       fg="grey",bty="n",xlab="Lag 1",ylab="Proportion in Favor of Leaving EU")
points(invlogit(X_short[lvout,"logit_p_lag1"][ph_Nmo]),
       invlogit(y_lvout)[ph_Nmo],
       col="dodgerblue",pch=20,cex=3)
points(invlogit(X_short[lvout,"logit_p_lag1"][Nph_mo]),
       invlogit(y_lvout)[Nph_mo],
       col="darkgreen",pch=20,cex=3)
points(invlogit(X_short[lvout,"logit_p_lag1"][Nph_Nmo]),
       invlogit(y_lvout)[Nph_Nmo],
       col="orange",pch=20,cex=3)
# Pred
points(invlogit(X_short[lvout,"logit_p_lag1"][ph_mo]),predx_ph_mo,
       pch=4,cex=2,lwd=3,col="red")
points(invlogit(X_short[lvout,"logit_p_lag1"][ph_Nmo]),predx_ph_Nmo,
       pch=4,cex=2,lwd=3,col="dodgerblue")
points(invlogit(X_short[lvout,"logit_p_lag1"][Nph_mo]),predx_Nph_mo,
       pch=4,cex=2,lwd=3,col="darkgreen")
points(invlogit(X_short[lvout,"logit_p_lag1"][Nph_Nmo]),predx_Nph_Nmo,
       pch=4,cex=2,lwd=3,col="orange")

legend("topleft",legend=c("Data: Phone, Ipsos",
                          "Data: Phone, Not Ipsos",
                          "Data: Online, Ipsos",
                          "Data: Online, Not Ipsos",
                          "Pred: Phone, Ipsos",
                          "Pred: Phone, Not Ipsos",
                          "Pred: Online, Ipsos",
                          "Pred: Online, Not Ipsos"), 
       pch=rep(c(20,4),each=4),pt.lwd=3,
       col=rep(c("red","dodgerblue","darkgreen","orange"),2),
       pt.cex=1, bty="n")
abline(h=.5,col="grey")
dev.off()

# Can't do this directly
#back_trans <- function(b,s=names(b)) {
#  invlogit( b/sd(X_short[,s]) * sd(y_short) )
#}
#back_trans(post.mean["Methodphone"])
#back_trans(post.mean["SourceIpsos Mori"])
