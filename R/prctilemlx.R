prctilemlx <- function(r,band=list(number=8,level=80),y.lim=NULL,plot=TRUE)
{
  alpha <- band$level
  m0 <- band$number
  q1=(1-alpha/100)/2
  q2=1-q1
  q=seq(q1,q2,length.out=m0+1)
  if (round(m0/2)!=m0/2){
    m=m0+1
    q=sort(c(q,0.5))
  }else{
    m=m0}
  N <- length(unique(r$id))
  n <- length(which(r$id==r$id[1]))
  d <- dim(r)[2]
  
  t <- t(r$time[1:n])
  y.label=names(r)[d]
  v <- matrix(r[,d], nrow = n, byrow = FALSE)
  
  y<-apply(v,1,quantile, probs = round(q,digits=3),  na.rm = TRUE)
  #   colnames(y)=as.character(round(t,digits=6))
  nq=length(q)
  ncol <- trunc((nq-1)/2)
  c=hsv(.8,.8,.7,seq(0.3,0.9,length.out=ncol))
  color <- c(c,rev(c))
  ty=cbind(round(t(t),digits=6),t(y))
  colnames(ty)[1]="time"
  dy <- as.data.frame(ty)
  if (m==m0){
    qr <- q
    colq <- color
  }else{
    qr=q[-(nq+1)/2]
    colq <- color[-(nq+1)/2]
  }
  
  res <- list(q=qr,color=colq,y=dy)
  
  if (plot==TRUE){
    if (is.null(y.lim))
      y.lim <- c(min(y[1,]),max(y[nq,]))
    
    plot(t,y[nq,],type="l",col=color[length(color)],xlab="",ylab="",ylim=y.lim)
    
    for (j in 1:(nq-1))
      polygon(c(t,rev(t)),c(y[j,],rev(y[j+1,])),col=color[j],border=NA)
    
    if (m==m0)
      points(t,y[(nq+1)/2,],type="l",col=hsv(.8,1,.6),lwd=2)
  }
  return(res)
}
