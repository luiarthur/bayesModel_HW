plot.per.county <- function(x, state.name,county.names, measure, dig=1,
                            col.pal=colorRampPalette(c("blue","white","red"))(3),
                            bks=NULL,percent=FALSE,paren=TRUE,text.names=TRUE,
                            text.cex=.7,num=NULL)
{

  mar <- par()$mar
  cp.len <- length(col.pal)
  cols <- rep(0,length(x))

  #qq <- quantile(x,(0:cp.len)/cp.len)
  qq <- quantile(x,seq(0,1,len=cp.len+1))
  if (!is.null(bks)) qq <- bks

  if (length(qq) != cp.len+1) {
    print("bks has to have length have col.pal + 1")
    return
  }

  for (i in 1:cp.len) {
    ind <- x >= qq[i]
    cols[ind] <- col.pal[i]
  } 

  cs <- paste0(state.name,",",county.names)
  map('county',state.name,col="grey90",mar=rep(0,4))
  map('county',cs,names=TRUE,add=TRUE,fill=TRUE,
      border="grey90",col=cols)

  if (text.names) {
    if (length(text.cex)==1) text.cex <- rep(text.cex,length(cs))
    if (!is.null(num)) county.names <- num
    for (i in 1:length(cs)) {
      rng <- map('county',cs[i],plot=FALSE)$range
      text((rng[1]+rng[2])/2, (rng[3]+rng[4])/2,county.names[i],cex=text.cex[i])
      # dat[i,1]...
      #rng <- map('county',cs[i],plot=FALSE)
      #rng <- c(mean(rng$x,na.rm=TRUE),mean(rng$y,na.rm=TRUE))
      #text(rng[1],rng[2],dat[i,1],cex=.7)
    }
  }

  #(leg.txt <- paste(">",round(quantile(x,c((cp.len-1):0)/cp.len),dig)))
  #leg.txt <- paste(">",round(rev(qq)[-1],dig))

  rq <- round(rev(qq),dig)
  leg.txt <- NULL
  if (percent) paren <- FALSE
  if (paren) {
    leg.txt <- paste0("(",rq[-1],", ",rq[-length(rq)],")")
  } else {
    leg.txt <- paste0(rq[-1],"-",rq[-length(rq)])
  }
  leg.txt[1] <- paste(">",rq[2])
  leg.txt[length(leg.txt)] <- paste("<",rq[length(rq)-1])
  leg.txt <- ifelse(rep(percent,length(leg.txt)),paste0(leg.txt,"%"),leg.txt)

  legend("topright",leg.txt,bty="n",pch=20,pt.cex=3,col=rev(col.pal),title=measure,
         cex=2)
  
  par(mar=mar)
}
