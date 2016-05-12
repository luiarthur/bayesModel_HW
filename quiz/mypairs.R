my.pairs <- function(M,digits=3) {
  cnames <- colnames(M)
  k <- ncol(M)
  corrs <- cor(M)
  par(mfrow=c(k,k),mar=c(0,0,0,0)+2)
  for (i in 1:k) {
    if (i>1) {
      for (j in 1:(i-1)) { 
        plot(1, type="n", axes=F, xlab="", ylab="",main="")
             #main=paste0("Corr (",cnames[i],", ",cnames[j],")")) # empty plot
        r <- round(corrs[i,j],digits)
        cex.cor <- max(.8/strwidth(format(r)) * abs(r),1)
        text(1,labels=r,cex=cex.cor,col="grey")
      }  
    }
    
    hist(M[,i],prob=TRUE,bty="n",fg="grey",col="grey",border="white",xlab="",
         main=cnames[i])
         #main=paste("Histogram of",cnames[i]))

    if (i<k) {
      for (j in (i+1):k) {
        plot(M[,c(j,i)],xlab=cnames[j],ylab=cnames[i],pch=20,
             bty="n",fg="grey",main="")
             #main=paste(cnames[i],"-",cnames[j])_
      }
    }  
  }
  par(mfrow=c(1,1))
}

#X <- matrix(rnorm(100),ncol=4)
#colnames(X) <- c("I","II","III","IV")
#my.pairs(X)

source("../R_Functions/plotPost.R",chdir=TRUE)
source("../R_Functions/plotinplot.R",chdir=TRUE)
simple.plot.post <- function(x,tckdig=3,cex.a=1,...) {
  maj.col <- "cornflowerblue"
  min.col <- col.mult(maj.col)
  xbar <- mean(x)
  den.x <- density(x)
  hpd.x <- get.hpd(x,a=.05,len=1e3)
  color.den(den.x,from=min(den.x$x),to=max(den.x$x),
            col.area=maj.col,col.den=maj.col,fg="grey",bty="n",xaxt="n",...)
  axis(1,at=c(hpd.x,xbar),labels=round(c(hpd.x,xbar),tckdig),las=0,fg="grey",
       cex.axis=cex.a)
  color.den(den.x,from=hpd.x[1],to=hpd.x[2],
            col.area=min.col,col.den=min.col,add=TRUE)
  lines(c(xbar,xbar),c(0,bound(xbar,den.x,ret=F)),lwd=2,col="red")
}

#par(mfrow=c(2,2),mar=c(0,0,0,0)+2)
#for (i in 1:4) {
#  simple.post.plot(rnorm(100),main="",xlab="",ylab="",cex.a=2)
#  plot.in.plot(minor.plot=function(x) plot(rnorm(100)),stay=F)
#}
#par(mfrow=c(1,1))

simple.plot.posts <- function(M,digits=3,tckdig=3,trace=TRUE,tracelab=FALSE,
                              cnames=colnames(M),...) {
  par.org <- par(no.readonly=TRUE)
  k <- ncol(M)
  corrs <- cor(M)
  par(mfrow=c(k,k),mar=c(0,0,0,-2)+2)
  for (i in 1:k) {
    if (i>1) {
      for (j in 1:(i-1)) { 
        plot(1, type="n", axes=F, xlab="", ylab="",main="")
        r <- round(corrs[i,j],digits)
        cex.cor <- max(.8/strwidth(format(r)) * abs(r),1)
        text(1,labels=r,cex=cex.cor,col="grey")
      }  
    }
    
    simple.plot.post(M[,i],main=cnames[i],tckdig=tckdig,...)
    if (trace) plot.in.plot(function(x) 
                            plot(M[,i],fg="grey",bty="n",col="grey",type='l',
                                 col.axis="grey",axes=tracelab),stay=F)

    if (i<k) {
      for (j in (i+1):k) {
        plot(M[,c(j,i)],xlab=cnames[j],ylab=cnames[i],pch=20,
             bty="n",fg="grey",main="",col="grey",type='l')
        plot.contour(M[,c(j,i)],col=col.mult("cornflowerblue"),add=TRUE)
      }
    }  
  }
  par(par.org)
}

#M <- matrix(rnorm(1000),ncol=4)
#colnames(M) <- 1:4
#simple.plot.posts(M)


add.errbar <- function(ci,transpose=FALSE,x=NULL,...) {
  if (any(is.null(x))) x <- 1:nrow(ci)
  if (!transpose)
    segments(x,ci[,1],x,ci[,2],...)
  else
    segments(ci[,1],x,ci[,2],x,...)
}

plot_err <- function(M,org,theft,mar=NULL,lab=TRUE,...) {
  par.org <- par()$mar
  if (is.null(mar)) mar=c(8,3,0,0)+1 
  par(mar=mar)
  ci <- t(apply(M,2,quantile,c(.025,.975)))
  plot(apply(M,2,mean),fg="grey",bty="n",xaxt="n",...)
  points(org,cex=2,col="red",pch=17)
  add.errbar(ci,col="grey",lwd=3)
  if (lab) axis(1,at=1:ncol(M),labels=colnames(M),las=2,fg='grey')
  else axis(1,at=1:ncol(M),lab=FALSE,las=2,fg='grey')
  mtext(theft,side=4,las=2)
  par(mar=par.org)
}
