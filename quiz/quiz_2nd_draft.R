source("mypairs.R")
source("../R_Functions/plotPost.R",chdir=TRUE)
source("gibbs.R",chdir=TRUE)
source("plotmap.R")
library(maps)
library(xtable)

# READ DATA: ######################################
system("mkdir -p report/figs")
dat <- read.csv("ca_theft.csv")
colnames(dat) <- gsub("\\."," ",colnames(dat))

logit <- function(p) log(p/(1-p))
inv.logit <- function(l) 1 / (1+exp(-l))

Y <- dat[,-c(1:2)]
X <- logit( Y / dat[,2] ) # logit(crimes per capita)

# VISUALIZE: ##############################################################
pdf("report/figs/pairsLogDat.pdf")
my.pairs(log(dat[,-1])) # Obviously, if number of one crime is high, 
                        # the number of any other crime will be high as well
                        # because there is a confounding with popluation.
dev.off()
pdf("report/figs/pairsLogRate.pdf")
my.pairs(X)             # A better measure of crime is crime per capita
dev.off()

county <- paste(dat[,1])
county[14] <- "Marin"
state <- "California"

col.pal <- colorRampPalette(c("chartreuse3","pink","brown1"))(5)
plot.per.county(log(dat[,2]),state,county,m="log Population",col=col.pal)
dev.off()

#source("plotmap.R")
#plot.per.county(dat[,3]/dat[,2]*100000,state,county,dig=0,text=T,bks=c(0,15,50,100,300,600),text.cex=tc,col=col.pal,paren=F,m="Robbery                 ",num=1:39)

bks <- c(0,15,50,100,300,600)
pdf("report/figs/mapDat.pdf",w=13,h=7)
par(mfrow=c(1,4))
plot.per.county(dat[,3]/dat[,2]*100000,state,county,dig=0,text=T,bks=NULL,text.cex=.5,col=col.pal,paren=F,m="Robbery                 ")
plot.per.county(dat[,4]/dat[,2]*100000,state,county,dig=0,text=T,bks=NULL,text.cex=.5,col=col.pal,paren=F,m="Burglary                ")
plot.per.county(dat[,5]/dat[,2]*100000,state,county,dig=0,text=T,bks=NULL,text.cex=.5,col=col.pal,paren=F,m="Larceny                 ")
plot.per.county(dat[,6]/dat[,2]*100000,state,county,dig=0,text=T,bks=NULL,text.cex=.5,col=col.pal,paren=F,m="Vehicle                 ")
par(mfrow=c(1,1))
dev.off()
#title("Thefts per 100000",cex.main=4,line=-2)

# Modeling log(Crime / Capita):  ################################
#priors.list <- list("m"=apply(X,2,mean),"s"=1,"S"=diag(4),"r"=10)
priors.list <- list("m"=rep(0,4),"s"=1,"S"=diag(4),"r"=10)
postpred <- sample.niw(X,priors.list,B=100000,print=TRUE)
postpred <- tail(postpred,10000)

post.mu <- t(sapply(postpred,function(xx) xx$mu))
post.S <- lapply(postpred,function(xx) xx$S)
post.pred <- t(sapply(postpred,function(xx) xx$postpred))

# Posterior Predictive:
plot.posts(post.pred,cex.a=1.3,cex.l=1,names=colnames(dat)[-c(1:2)])

# Posterior Mu
source("mypairs.R")
colnames(post.mu) <- colnames(Y)
pdf("report/figs/postMu.pdf",w=13,h=13)
simple.plot.posts(post.mu,cex.a=1.4,tck.dig=2)
dev.off()

# Posterior for S
(post.S.mean <- func.matrices(post.S,mean))
(post.S.sd <- func.matrices(post.S,sd))
(post.S.lo <- func.matrices(post.S,function(x) quantile(x,.025)))
(post.S.hi <- func.matrices(post.S,function(x) quantile(x,.975)))

sink("report/figs/postmeanS.tex")
  xtab <- xtable(post.S.mean,dig=3)
  print(xtab,floating=FALSE,tabular.environment="array",hline.after=NULL,
        include.rownames=FALSE,include.colnames=FALSE)
sink()
sink("report/figs/postLoS.tex")
  xtab <- xtable(post.S.lo,dig=3)
  print(xtab,floating=FALSE,tabular.environment="array",hline.after=NULL,
        include.rownames=FALSE,include.colnames=FALSE)
sink()
sink("report/figs/postHiS.tex")
  xtab <- xtable(post.S.hi,dig=3)
  print(xtab,floating=FALSE,tabular.environment="array",hline.after=NULL,
        include.rownames=FALSE,include.colnames=FALSE)
sink()

#x=xtable(C4,align=rep("",ncol(C4)+1),digits=2)
#print(x,floating=FALSE,tabular.environment="pmatrix",hline.after=NULL,
#      include.rownames=FALSE,include.colnames=FALSE)


# Postpred. Expected Number of Each Theft in Each County.
apply(Y,2,quantile)
bk.r <- seq(0,200,len=6)
bk.b <- seq(0,900,len=6)
bk.l <- seq(0,1400,len=6)
bk.m <- seq(0,180,len=6)
  
list.ipp <- lapply(as.list(dat[,2]), function(pop) pop * inv.logit(post.pred))
pm.crime <- t(sapply(list.ipp,apply,2,mean))


ord <- order(apply(Y,1,sum))
tmp1 <- sapply(list.ipp,function(x) x[,1])
tmp2 <- sapply(list.ipp,function(x) x[,2])
tmp3 <- sapply(list.ipp,function(x) x[,3])
tmp4 <- sapply(list.ipp,function(x) x[,4])
colnames(tmp1) <- county
colnames(tmp2) <- county
colnames(tmp3) <- county
colnames(tmp4) <- county

pdf("report/figs/expTheftCounty.pdf",w=13,h=7)
source("mypairs.R")
par(mfrow=c(4,1),oma=c(5,0,0,3)+2)
plot_err(tmp1[,ord],Y[ord,1],"Robbery",col="blue",pch=20,cex=3,xlab="",mar=c(0,0,0,0)+2,lab=F)
plot_err(tmp2[,ord],Y[ord,2],"Burglary",col="blue",pch=20,cex=3,xlab="",mar=c(0,0,0,0)+2,lab=F)
plot_err(tmp3[,ord],Y[ord,3],"Larceny",col="blue",pch=20,cex=3,xlab="",mar=c(0,0,0,0)+2,lab=F)
plot_err(tmp4[,ord],Y[ord,4],"Vehicle",col="blue",pch=20,cex=3,xlab="",mar=c(0,0,0,0)+2)
par(mfrow=c(1,1))
dev.off()

pdf("report/figs/expAllTheft.pdf",w=13,h=5)
par(oma=c(5,0,0,3)+2)
plot_err(tmp1[,ord]+tmp2[,ord]+tmp3[,ord]+tmp4[,ord],apply(Y[ord,],1,sum),"All Thefts",col="blue",pch=20,cex=3,ylab="",xlab="",mar=c(0,0,0,0)+2)
dev.off()


par(mfrow=c(2,4))
plot.per.county(pm.crime[,1],state,county,dig=0,paren=F,bks=bk.r,col=col.pal,m="Posterior Predictive - Robbery")
plot.per.county(pm.crime[,2],state,county,dig=0,paren=F,bks=bk.b,col=col.pal,m="Posterior Predictive - Burglary")
plot.per.county(pm.crime[,3],state,county,dig=0,paren=F,bks=bk.l,col=col.pal,m="Posterior Predictive - Larceny")
plot.per.county(pm.crime[,4],state,county,dig=0,paren=F,bks=bk.m,col=col.pal,m="Posterior Predictive - Vehicle")
plot.per.county(Y[,1],state,county,dig=0,bks=bk.r,col=col.pal,paren=F,m="Data - Robbery                   ")
plot.per.county(Y[,2],state,county,dig=0,bks=bk.b,col=col.pal,paren=F,m="Data - Burglary                  ")
plot.per.county(Y[,3],state,county,dig=0,bks=bk.l,col=col.pal,paren=F,m="Data - Larceny                   ")
plot.per.county(Y[,4],state,county,dig=0,bks=bk.m,col=col.pal,paren=F,m="Data - Vehicle                   ")
par(mfrow=c(1,1))

pm.crime

# Postpred. Expected Total Number of Thefts in each county.
pdf("report/figs/tmp.pdf",w=17,h=9)
bks <- round( quantile(apply(Y,1,sum),seq(0,1,len=length(col.pal)+1)) )
par(mfrow=c(1,3))
plot.per.county(apply(Y,1,sum),state,county,dig=0,bks=bks,col=col.pal,paren=F,m="Data - Thefts               ")
plot.per.county(apply(pm.crime,1,sum),state,county,dig=0,paren=F,bks=bks,col=col.pal,m="Posterior Predictive - Thefts")
plot.per.county(apply(Y,1,sum) - apply(pm.crime,1,sum),bks=NULL,state,county,dig=0,col=col.pal,paren=T,m="Data minus Posterior Predictive          ")
par(mfrow=c(1,1))
dev.off()

# Santa Cruz Thefts
pp.sc <- list.ipp[[31]] #apply(post.pred,2,function(x) inv.logit(x) * dat[31,2])

pp.sum.sc <- apply(pp.sc,1,sum)

pdf("report/figs/sc.pdf")
color.den(density(pp.sum.sc,from=0),from=-5,to=4000,lwd=1,bty="n",main="",
          col.area="cornflowerblue",col.den="cornflowerblue",fg="grey",xlim=c(0,10000))
color.den(density(pp.sum.sc),from=3000,to=50000,col.area="blue",add=TRUE,col.den="blue",lwd=6)
(prob.sc.above.3000 <- mean(pp.sum.sc > 3000)) # 12.7 %
dev.off()
#text(4000,1e-04, paste0("Prob. Santa Cruz Thefts > 3000 is ",prob.sc.above.3000*100,"%"),cex=2)
