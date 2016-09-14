dat <- read.csv("../dat/fye.dat",header=T)

# (a)
cMR <- dat[,"ctrlMortRate"]
tMR <- dat[,"trtMortRate"]
y <- cMR - tMR
k <- nrow(dat) # number of studies

t(apply(rbind(cMR, tMR, y),1,summary))
a_out <- t(rbind(cMR,tMR,y))
colnames(a_out) <- c("Ctrl_MR","Trt_MR","y")
rownames(a_out) <- dat$study
a_out

plot(a_out[,1],pch=16,type='o',cex=1.5,lwd=3,col="red",
     ylim=range(a_out),fg='grey',bty='n',
     ylab="Mortality Rate (%)", xlab="Study",
     xaxt="n")
lines(a_out[,2],pch=16,type='o',cex=1.5,lwd=3,col="blue")
lines(a_out[,3],pch=16,type='o',cex=1.5,lwd=3,col="orange")
axis(1,fg='grey',at=1:k,labels=dat$study)
abline(v=1:k,col="grey",lty=3)
legend("topleft",legend=c("Control","Treatment","y=(C-T)"),
       text.col=c("red","blue","orange"),bty="n",text.font=2)


# what is tMR?
# what is cMR?
# what is y?
