#' Percentiles of the empiricial distribution of longitudinal data
#'
#' Compute and display percentiles of the empiricial distribution of longitudinal data.
#'
#' See http://simulx.webpopix.org/mlxr/prctilemlx/ for more details.
#' @param r a data frame with a column \samp{id}, a column \samp{time} and a column with values.
#' The times should be the same for each individual. 
#' @param col a vector of 3 column numbers: (\samp{id}, \samp{time/x}, \samp{y}. Default = c(1, 2,3).
#' @param number the number of intervals (i.e. the number of percentiles minus 1).
#' @param level the largest interval (i.e. the difference between the lowest and the highest percentile).
#' @param plot if \code{TRUE} the empirical distribution is displayed, if \code{FALSE}
#' @param color a color (default="purple")
#' the values of the percentiles are returned
#' 
#' @return 
#' a ggplot object if \code{plot=TRUE} ; otherwise, a list with fields:
#' \itemize{
#'   \item proba a vector of probabilities of length \code{band$number+1} 
#'   \item color a vector of colors used for the plot of length \code{band$number}
#'   \item y a data frame with the values of the empirical percentiles computed at each time point
#' }
#' @examples
#' \dontrun{
#'   myModel <- inlineModel("
#'   [LONGITUDINAL]
#'   input = {ka, V, Cl}
#'   EQUATION:
#'   C = pkmodel(ka,V,Cl)
#'   
#'   [INDIVIDUAL]
#'   input = {ka_pop, V_pop, Cl_pop, omega_ka, omega_V, omega_Cl}
#'   DEFINITION:
#'   ka = {distribution=lognormal, reference=ka_pop, sd=omega_ka}
#'   V  = {distribution=lognormal, reference=V_pop,  sd=omega_V }
#'   Cl = {distribution=lognormal, reference=Cl_pop, sd=omega_Cl}
#'   ")
#'   
#'   N=2000
#'   
#'   pop.param   <- c(
#'     ka_pop  = 1,    omega_ka  = 0.5,
#'     V_pop   = 10,   omega_V   = 0.4,
#'     Cl_pop  = 1,    omega_Cl  = 0.3)
#'     
#'   res <- simulx(model     = myModel,
#'                 parameter = pop.param,
#'                 treatment = list(time=0, amount=100),
#'                 group     = list(size=N, level='individual'),
#'                 output    = list(name='C', time=seq(0,24,by=0.1)))
#'   # res$C is a data.frame with 2000x241=482000 rows and 3 columns
#'   res$C[1:10,]
#'   # we can display the empirical percentiles of C using the default 
#'   # settings (i.e. percentiles of order 10%, 20%, ... 90%)
#'   p1   <- prctilemlx(res$C)
#'   print(p1)
#'   # The 3 quartiles (i.e. percentiles of order 25%, 50% and 75%) are displayed by 
#'   # selecting a 50% interval splitted into 2 subintervals
#'   p2  <- prctilemlx(res$C, band=list(number=2, level=50))
#'   # A one 90% interval can be displayed using only one interval
#'   p3   <- prctilemlx(res$C, band=list(number=1, level=90))
#'   # or 75 subintervals in order to better represent the continuous distribution 
#'   # of the data within this interval
#'   p4   <- prctilemlx(res$C, band=list(number=75, level=90))
#'   # The percentiles are not plotted by setting plot=FALSE
#'   p5   <- prctilemlx(res$C, band=list(number=4, level=80), plot=FALSE)
#'   print(names(p4))
#'   print(p4$proba)
#'   print(p4$color)
#'   print(p4$y[1:5,])
#'   print(p4$y[1:5,])
#' }
#' @importFrom stats quantile
#' @export         
prctilemlx <- function(r,col=c(1,2,3), number=8,level=80,plot=TRUE,color="purple",band=NULL,y.lim=NULL)
{
  col.hsv <- rgb2hsv(col2rgb(color))
  if (!is.null(y.lim))
    warning("The use of y.lim is deprecated. You can use  prctile(...) +ylim(...) instead.")
  
  if (!is.null(band)) {
    level <- band$level
    number <- band$number
  } 
  m <- number
  
  q1=(1-level/100)/2
  q2=1-q1
  
  time=NULL
  if (m%%2!=0){
    m.test <- 0
    q=seq(q1,q2,length.out=(m+1))
    q <- append(q,0.5,(m+1)/2) 
    m <- m+1
  }else{
    m.test <- 1
    q=seq(q1,q2,length.out=m+1)
    q <- append(q,0.5,m/2) 
    q[m/2+2] <- 0.5
    m <- m+1
  }
  
  r.names <- names(r)
  if (any(r.names=="id"))
    col[1] <- which(r.names=="id")
  if (any(r.names=="time"))
    col[2] <- which(r.names=="time")
  
  id <- r[,col[1]]
  N <- length(unique(id))
  n <- length(which(id==id[1]))
  d <- col[3]
  
  t <- r[,col[2]][1:n]
  x.label=names(r)[col[2]]
  y.label=names(r)[col[3]]
  v <- matrix(r[,col[3]], nrow = n, byrow = FALSE)
  
  y<-apply(v,1,quantile, probs = round(q,digits=3),  na.rm = TRUE)
  nq=length(q)
  ncol <- trunc(nq/2)
  if (number<=2){
    cl=hsv(col.hsv[1],col.hsv[2],col.hsv[3],0.4)
  }else{
    cl=hsv(col.hsv[1],col.hsv[2],col.hsv[3],seq(0.2,0.8,length.out=ncol)) 
  }
  if (m.test==1)
    cl[ncol] <- hsv(col.hsv[1],col.hsv[2],col.hsv[3],1)
  color <- c(cl,rev(cl))
  
  ty=cbind(round(t,digits=6),t(y))
  colnames(ty)[1]="time"
  dy <- as.data.frame(ty)
  if (m%%2==0){
    colq <- color
  }else{
    colq <- color[-(nq+1)/2]
  }
  
  res <- list(proba=q,color=colq,y=dy)
  
  if (plot==TRUE){
    nt <- length(t)
    if (m%%2==0){
      q <- q[-(nq+1)/2]
    }else{
      q[nq/2] <- (q[nq/2]+q[nq/2+1])/2
      q <- q[-(nq/2+1)]
    }
    q[nq] <- 1
    q <- round(q,2)
    vf <- rep(as.factor(q[(nq-1):1]), each=2*nt)
    x <- rep(c(t,rev(t)),nq-1)
    
    nbq.max <- 13
    if (nq>nbq.max){
      if (m.test==0){
        iq1 <- round(seq(1,nq/2,length.out=(nbq.max+1)/2))
      }else{
        iq1 <- round(seq(1,nq/2,length.out=(nbq.max+1)/2))
      }
      iq2 <- nq-iq1
      iq <- sort(unique(c(iq1,iq2)))
      vq <- q[iq]
    }else{
      vq <- q[1:nq]
    }
    
    bq <- as.character(rev(vq))
    sfm = scale_fill_manual(name="proba",values=colq,breaks=bq)
    
    pr <- NULL
    for (j in (1:(nq-1)))
      pr <- c(pr,y[j,],rev(y[j+1,]))
    
    datapoly <- data.frame(x,pr,v,vf)    
    # pk<-ggplotmlx(datapoly, aes(x=x, y=pr)) + geom_polygon(aes(fill=vf, group=vf)) +
      pk<-ggplotmlx() 
    pk<-pk + geom_polygon(data=datapoly, aes(x=x, y=pr, fill=vf, group=vf)) +
      xlab(x.label)+ylab(y.label) 
    if (!is.null(y.lim))
      pk <- pk + ylim(y.lim) 
    pk <- pk +sfm
    if (m.test==1){
      data0 <- data.frame(y=y[(nq+1)/2,],x=t)
      pk <- pk+ geom_line(data=data0, aes(x=x,y=y),col=hsv(col.hsv[1],col.hsv[2],col.hsv[3],1))
    }
    res <- pk
  }
  return(res)
}

