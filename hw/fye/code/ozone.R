set.seed(207)
library(rjulia)
source("plotmap.R")
source("../../quiz/mypairs.R",chdir=TRUE)
system("mkdir -p ../report/figs")

# Data:
dat <- read.table("../dat/ozone.dat",header=TRUE)
nrow(dat)
colnames(dat)
# ozone concentration: ppb
#     solar radiation: 0-low; 1-moderate to high
#      daily max temp: F
#          wind speed: mph


# EXPLORATORY ANALYSIS & FINDINGS
pdf("../report/figs/pairs.pdf")
  my.pairs(dat)
dev.off()

y <- log(dat$ozone)
log_dat <- cbind(y,dat[,-1])
colnames(log_dat)[1] <- "log_ozone"
head(log_dat)

pdf("../report/figs/log_ozone_pairs.pdf")
  my.pairs(log_dat)
dev.off()


# MODEL FITTING:

# Julia Settings:
julia_init()
julia_void_eval('include("julia/g-prior.jl")')

# Fit Model:
gprior <- function(y,X,B=2000,g=nrow(X),seed=207) {
  r2j(y,"y")
  r2j(X,"X_tmp")
  r2j(g,"g")
  r2j(seed,"seed")
  r2j(B,"B")

  julia_void_eval("X = convert(Matrix,X_tmp)")
  julia_void_eval("B = Int64(B[1])")
  julia_void_eval("g = Int64(g[1])")
  julia_void_eval("seed = Int64(seed[1])")
  julia_void_eval("blas_set_num_threads(1)")

  julia_void_eval("@time out = gprior(y,X,B,add_intercept=true,
                  setseed=seed,g=g)")

  post.phi <- j2r("out[:phi]")
  post.beta <- j2r("out[:beta]")

  linear.mod <- lm(y~.,data=X)

  n <- nrow(X)
  p <- ncol(X)
  R2 <- summary(linear.mod)$r.squared
  #BF <- (1+g)^((n-p-1)/2) * (1+g*(1-R2))^(-(n-1)/2)
  logBF <- ((n-p-1)/2) * log(1+g) + (-(n-1)/2) * log(1+g*(1-R2))

  list("phi"=post.phi,"beta"=post.beta,"lm"=linear.mod,"logBF"=logBF)
}

test <- sample(1:nrow(dat),round(nrow(dat)*.3))
mod1 <- gprior(y[-test],log_dat[-test,-c(1:2)],B=2000)
mod2 <- gprior(y[-test],log_dat[-test,-1],B=2000)

# INTERACTION
XInt <- model.matrix(log_ozone~.^2-1,data=log_dat)
attr(XInt,"assign") <- NULL
modIntFull <- gprior(y[-test], as.data.frame(XInt[-test,]),B=2000)
modIntRT<- gprior(y[-test], as.data.frame(XInt[-test,-c(5:6)]),B=2000)
modIntRW<- gprior(y[-test], as.data.frame(XInt[-test,-c(4,6)]),B=2000)
modIntTW<- gprior(y[-test], as.data.frame(XInt[-test,-c(4,5)]),B=2000)
modIntFull$logBF
modIntRT$logBF
modIntRW$logBF
modIntTW$logBF
# END OF INTERACTIONS


# Larger BF is better
mod1$logBF # 34.52
mod2$logBF # 40.40
(BF21 <- mod2$logBF - mod1$logBF) # 6.18 => positive => prefer mod2
c(.5,1,2) * 2.303 #=> substantial, strong, decisive


pdf("../report/figs/posts1.pdf")
simple.plot.posts(cbind(mod1$phi,mod1$beta),ma=2,tckdig=2,cex.a=.6,
                  cnames=c("phi","Intercept",colnames(log_dat[,-c(1:2)])))
dev.off()

pdf("../report/figs/posts2.pdf")
simple.plot.posts(cbind(mod2$phi,mod2$beta),ma=2,tckdig=2,cex.a=.6,
                  cnames=c("phi","Intercept",colnames(log_dat[,-1])))
dev.off()

y_pred_1 <- t(apply(mod1$beta,1,function(b) 
                  as.matrix(cbind(1,dat[test,-c(1:2)])) %*% matrix(b)))
y_pred_2 <- t(apply(mod2$beta,1,function(b) 
                  as.matrix(cbind(1,dat[test,-c(1)])) %*% matrix(b)))

hpd1 <- apply(y_pred_1,2,get.hpd)
hpd2 <- apply(y_pred_2,2,get.hpd)

'%bw%' <- function(a,b) a>b[1] && a<b[2]

coverage1 <- mean(sapply(1:ncol(hpd1),function(i) y[test][i] %bw% hpd1[,i]))
coverage2 <- mean(sapply(1:ncol(hpd2),function(i) y[test][i] %bw% hpd2[,i]))

pdf("../report/figs/obsvsfit.pdf")
plot(y[test],apply(y_pred_1,2,mean),ylim=c(0,5),xlim=c(0,5),
     col=rgb(0,0,1,1),pch=20,cex=2,fg="grey",bty="n",
     xlab="Observed",ylab="Predicted")
points(y[test],apply(y_pred_2,2,mean),ylim=c(0,5),xlim=c(0,5),
       col=rgb(1,0,0,1),pch=20,cex=2)
add.errbar(t(hpd1),x=y[test],col=rgb(0,0,1,.8),lwd=4)
add.errbar(t(hpd2),x=y[test],col=rgb(1,0,0,.8),lwd=4)
abline(a=0,b=1,col="grey")
legend("topleft",col=c("blue","red"),lwd=4,bty="n",
       legend=c("Model without Radiation","Model with Radiation"))
dev.off()

#resid1 <- apply(y_pred_1,1,function(yy) (yy - y[test]))
#resid2 <- apply(y_pred_2,1,function(yy) (yy - y[test]))
#plot(apply(resid1,1,mean))
#plot(apply(resid2,1,mean))

rmse1 <- apply(y_pred_1,1,function(yy) sd(yy - y[test]))
rmse2 <- apply(y_pred_2,1,function(yy) sd(yy - y[test]))

pdf("../report/figs/rmse.pdf")
plot(density(rmse1),col="transparent",ylim=c(0,15),xlim=c(.51,.8),
     bty="n",fg="grey",main="")
lines(density(rmse2),col="transparent")
color.den(density(rmse2),0,10,add=TRUE,col.den=rgb(1,0,0,.6),
          col.area=rgb(1,0,0,.6))
color.den(density(rmse1),0,10,add=TRUE,col.den=rgb(0,0,1,.6),
          col.area=rgb(0,0,1,.6))
legend("topright",col=c("blue","red"),lwd=4,bty="n",cex=1,
       legend=c("Model without Radiation","Model with Radiation"))
dev.off()

mean(rmse2<rmse1) # P[RMSE2 < RMSE1] = .8375

# Plot a heatmap of the concentration change with radiation = 0, 1

constr.seq <- function(x,l=100) seq(min(x),max(x),len=l)
temp <- constr.seq(dat$temp)
wind <- constr.seq(dat$wind)
s <- expand.grid(temp,wind) #x,y
ypred1s <- t(apply(mod1$beta,1,function(b) 
                  as.matrix(cbind(1,s)) %*% matrix(b)))
ypred20s <- t(apply(mod2$beta,1,function(b) 
                    as.matrix(cbind(1,0,s)) %*% matrix(b)))
ypred21s <- t(apply(mod2$beta,1,function(b) 
                    as.matrix(cbind(1,1,s)) %*% matrix(b)))

pdf("../report/figs/map.pdf")
bks <- log(c(10,50)) # chosen so that dark red is log(70) => dangerous
rad <- which(dat$radiation==1)
par(mfrow=c(3,1))
  plotmap(apply(ypred1s,2,mean),s,bks=bks,ylab="Wind Speed (mph)",
          main="Model without Radiaion",col.main="grey30")
  plotmap(y,cbind(dat$temp,dat$wind),bks=bks,ylab="Wind Speed (mph)",add=T)

  plotmap(apply(ypred20s,2,mean),s,bks=bks,ylab="Wind Speed (mph)",
          main="Model with Radiation = 0",col.main="grey30")
  plotmap(y[-rad],cbind(dat$temp[-rad],dat$wind[-rad]),bks=bks,ylab="Wind Speed (mph)",add=T)

  plotmap(apply(ypred21s,2,mean),s,bks=bks,ylab="Wind Speed (mph)",
          main="Model with Radiation = 1",col.main="grey30",
          xlab="Temperature (F)")
  plotmap(y[rad],cbind(dat$temp[rad],dat$wind[rad]),bks=bks,ylab="Wind Speed (mph)",add=T)
par(mfrow=c(1,1))
dev.off()

customUpper <- function(i,j,M) {
  plot(M[,c(j,i)],bty="n",fg="grey",pch=20)
  n <- length(temp)
  W <- cbind(1,sample(0:1,n,replace=TRUE),sample(temp),sample(wind))
  yhat <- t(apply(mod2$beta,1,function(b) W %*% matrix(b)))
  if (i==1) {
    points(W[,j],apply(yhat,2,mean),col=rgb(0,0,1,.8),pch=20,cex=.5)
  }
}
pdf("../report/figs/marginal.pdf")
my.pairs(log_dat,customUpper=customUpper)
dev.off()
