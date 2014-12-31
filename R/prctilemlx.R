#' Compute and display percentiles of the empiricial distribution of longitudinal data
#'
#' @param r a data frame with a column \samp{id}, a column \samp{time} and a column with values.
#' The times should be the same for each individual. 
#' @param band a list with two fields
#' \itemize{
#'   \item \code{number} the number of intervals (i.e. the number of percentiles minus 1).
#'   \item \code{level} the largest interval (i.e. the difference between the lowest and the highest percentile).
#' }
#' @param y.lim vector of length 2, giving the y coordinate range
#' @param plot if \code{TRUE} the empirical distribution is displayed, if \code{FALSE}
#' the values of the percentiles are returned
#' 
#' @return 
#' a list with fields:
#' \itemize{
#'   \item q a vector of percentiles of length \code{band$number+1} 
#'   \item color a vector of colors used for the plot of length \code{band$number}
#'   \item y a data frame with the values of the empirical percentiles computed at each time point
#' }
#' @examples
#' myModel <- inlineModel("
#' [LONGITUDINAL]
#' input = {ka, V, Cl}
#' EQUATION:
#' C = pkmodel(ka,V,Cl)
#' 
#' [INDIVIDUAL]
#' input = {ka_pop, V_pop, Cl_pop, omega_ka, omega_V, omega_Cl}
#' DEFINITION:
#' ka = {distribution=lognormal, reference=ka_pop, sd=omega_ka}
#' V  = {distribution=lognormal, reference=V_pop,  sd=omega_V }
#' Cl = {distribution=lognormal, reference=Cl_pop, sd=omega_Cl}
#' ")
#' 
#' N=2000
#' 
#' pop.param   <- c(
#'   ka_pop  = 1,    omega_ka  = 0.5,
#'   V_pop   = 10,   omega_V   = 0.4,
#'   Cl_pop  = 1,    omega_Cl  = 0.3)
#'   
#' res <- simulx(model     = myModel,
#'               parameter = pop.param,
#'               treatment = list(time=0, amount=100),
#'               group     = list(size=N, level='individual'),
#'               output    = list(name='C', time=seq(0,24,by=0.1)))
#' # res$C is a data.frame with 2000x241=482000 rows and 3 columns
#' res$C[1:10,]
#' # we can compute and display the empirical percentiles of C using the default 
#' # settings (i.e. percentiles of order 10%, 20%, ... 90%)
#' p   <- prctilemlx(res$C)
#' print(names(p))
#' print(p$q)
#' print(p$color)
#' print(p$y[1:5,])
#' # The 3 quartiles (i.e. percentiles of order 25%, 50% and 75%) are displayed by 
#' # selecting a 50% interval splitted into 2 subintervals
#' p  <- prctilemlx(res$C, band=list(number=2, level=50))
#' # A one 90% interval can be displayed using only one interval
#' p   <- prctilemlx(res$C, band=list(number=1, level=90))
#' # or 99 subintervals in order to better represent the continuous distribution 
#' # of the data within this interval
#' p   <- prctilemlx(res$C, band=list(number=99, level=90))
#' # The percentiles are not plotted by setting plot=FALSE
#' p   <- prctilemlx(res$C, band=list(number=4, level=80), plot=FALSE)
#' print(p$y[1:5,])
#' @export         
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
