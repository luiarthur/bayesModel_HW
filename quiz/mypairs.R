my.pairs <- function(M,digits=3) {
  cnames <- colnames(M)
  k <- ncol(M)
  corrs <- cor(M)
  par(mfrow=c(k,k))
  for (i in 1:k) {
    if (i>1) {
      for (j in 1:(i-1)) { 
        plot(1, type="n", axes=F, xlab="", ylab="",
             main=paste0("Corr (",cnames[i],",",cnames[j],")")) # empty plot
        r <- round(corrs[i,j],digits)
        cex.cor <- max(.8/strwidth(format(r)) * abs(r),1)
        text(1,labels=r,cex=cex.cor)
      }  
    }
    
    hist(M[,i],prob=TRUE,main=paste("Histogram of",cnames[i]))

    if (i<k) {
      for (j in (i+1):k) {
        plot(M[,c(j,i)],xlab=cnames[j],ylab=cnames[i],pch=20,
             main=paste(cnames[i],"-",cnames[j]))
      }
    }  
  }
  par(mfrow=c(1,1))
}

my.pairs(matrix(rnorm(100),ncol=4))
