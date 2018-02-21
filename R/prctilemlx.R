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
#' the values are returned
#' @param color a color to be used for the plots (default="#9a35ff")
#' @param group  variable to be used for defining groups (by default, \samp{group} is used when it exists)
#' @param facet  makes subplots for different groups if \code{TRUE} 
#' @param labels  vector of strings 
#' @param band is deprecated (use number and level instead) ; a list with two fields
#' \itemize{
#'   \item \code{number} the number of intervals (i.e. the number of percentiles minus 1).
#'   \item \code{level} the largest interval (i.e. the difference between the lowest and the highest percentile).
#' }
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
#'   head(res$C)
#'   # we can display the empirical percentiles of C using the default 
#'   # settings (i.e. percentiles of order 10%, 20%, ... 90%)
#'   prctilemlx(res$C)
#'   # The 3 quartiles (i.e. percentiles of order 25%, 50% and 75%) are displayed by 
#'   # selecting a 50% interval splitted into 2 subintervals
#'   prctilemlx(res$C, number=2, level=50)
#'   # A one 90% interval can be displayed using only one interval
#'   prctilemlx(res$C, number=1, level=90)
#'   # or 75 subintervals in order to better represent the continuous distribution 
#'   # of the data within this interval
#'   prctilemlx(res$C, number=75, level=90)
#'   # prctilemlx produces a ggplot object that can be modified
#'   pl <- prctilemlx(res$C, number=4, level=80) 
#'   pl + ylab("concentration") + ggtitle("predictive distribution")
#'   # The percentiles are not plotted by setting plot=FALSE
#'   pr.out <- prctilemlx(res$C, number=4, level=80, plot=FALSE)
#'   print(pr.out$proba)
#'   print(pr.out$color)
#'   print(pr.out$y[1:5,])
#' }
#' @importFrom ggplot2 ggplot geom_point theme aes geom_line xlab ylab facet_wrap facet_grid
#' @importFrom stats quantile 
#' @export         
prctilemlx <- function(r,col=NULL, number=8, level=80, plot=TRUE, color="#9a35ff",
                       group=NULL, facet=TRUE, labels=NULL, band=NULL)
{
  
  if (!is.null(band)) {
    level <- band$level
    number <- band$number
  } 
  m <- number
  
  if (level<1) level=100*level
  
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
  
  if (is.null(col)) {
    r.names <- names(r)
    if (any(r.names=="id")) {
      col[1] <- which(r.names=="id")
    } else {
      col[1] <- 1
    }
    if (any(r.names=="time")) {
      col[2] <- which(r.names=="time")
    } else {
      col[2] <- 2
    }
    if (!is.null(attr(r,"name"))) {
      col[3] <- max(which(r.names==attr(r,"name")))
    } else {
      col[3] <- 3
    }
  }
  
  id <- r[,col[1]]
  d <- col[3]
  t <- unique(r[,col[2]])
  n <- length(t)
  
  # n <- length(which(id==id[1]))
  # t <- r[,col[2]][1:n]
  x.label=names(r)[col[2]]
  y.label=names(r)[col[3]]
  
  nq=length(q)
  ncol <- trunc(nq/2)
  col.hsv <- rgb2hsv(col2rgb(color))
  if (number<=2){
    cl=hsv(col.hsv[1],col.hsv[2],col.hsv[3],0.4)
  }else{
    cl=hsv(col.hsv[1],col.hsv[2],col.hsv[3],seq(0.2,0.8,length.out=ncol)) 
  }
  if (m.test==1)
    cl[ncol] <- hsv(col.hsv[1],col.hsv[2],col.hsv[3],1)
  color <- c(cl,rev(cl))
  
  if (m%%2==0){
    colq <- color
  }else{
    colq <- color[-(nq+1)/2]
  }
  
  if (!is.null(r$group) & is.null(group)) {
    group <- "group"
  } else if (length(group)==1 && group=="none") {
    group = NULL
  }
  
  if (is.data.frame(group)) {
    attr.name <- attr(r,"name")
    r <- merge(r,group,by="id",sort=FALSE)
    attr(r,"name") <- attr.name
    group <- names(group)
    group <- group[-which(group=="id")]
  }
  
  if (!is.null(labels)) {
    if (length(group)==1) 
      labels <- ifelse(is.list(labels),labels, list(labels))
    for (k in (1: length(group)))
      r[[group[k]]] <- factor(r[[group[k]]], labels=labels[[k]])
  }
  
  if (!is.null(group)) {
    ig <- interaction(r[group])
  } else {
    n.tot <- dim(r)[1]
    ig <- rep(1, n.tot)
  }
  ig <- factor(ig)
  ug <- levels(ig)
  ng <- length(ug)
  y <- list()
  for (k in (1:ng)) {
    jk <- which(ig==ug[k])
    vk <- matrix(r[jk,col[3]], nrow = n, byrow = FALSE)
    y[[k]] <- apply(vk,1,quantile, probs = round(q,digits=3),  na.rm = TRUE)
  }
  
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
    
    datapoly <- NULL
    pk<-ggplotmlx()
    for (k in (1:ng)) {
      pr <- NULL
      for (j in (1:(nq-1)))
        pr <- c(pr,y[[k]][j,],rev(y[[k]][j+1,]))
      dk <- data.frame(x,pr,vf)
      if (!is.null(group)) {
        jk <- which(ig==ug[k])
        dk[group] <- r[group][jk[1],]
      }
      datapoly <- rbind( datapoly, dk)
      pk<-pk + geom_polygon(data=dk, aes(x=x, y=pr, fill=vf, group=vf)) 
    }
    # pk<-pk + geom_polygon(data=datapoly, aes(x=x, y=pr, fill=vf, group=vf)) 
    pk<-pk +  xlab(x.label)+ylab(y.label)
    
    
    pk<-pk + xlab(x.label)+ylab(y.label) 
    
    pk <- pk +sfm
    if (m.test==1){
      #      data0 <- NULL
      for (k in (1:ng)) {
        datak <- data.frame(y=y[[k]][(nq+1)/2,],x=t)
        if (!is.null(group)) {
          jk <- which(ig==ug[k])
          datak[group] <- r[group][jk[1],]
        }
        pk <- pk+ geom_line(data=datak, aes(x=x,y=y),col=hsv(col.hsv[1],col.hsv[2],col.hsv[3],1))
        #        data0 <- rbind(data0, datak)
      }
      #      pk <- pk+ geom_line(data=data0, aes(x=x,y=y),col=hsv(col.hsv[1],col.hsv[2],col.hsv[3],1))
    }
    if (facet==TRUE) {
      if (length(group)==1)
        pk <- pk + facet_wrap(group)
      if (length(group)==2)
        pk <- pk + facet_grid(paste(group[1],"~",group[2]))
    }
    res <- pk
  } else {
    dy <- NULL
    for (k in (1:ng)) {
      tyk <- as.data.frame(cbind(round(t,digits=6),t(y[[k]])))
      if (!is.null(group) && nlevels(factor(r[[group]]))>1) {
        jk <- which(ig==ug[k])
        tyk[group] <- factor(r[group][jk[1],])
      }
      dy=rbind(dy, tyk)
    }
    names(dy)[1]="time"
    res <- list(proba=q,color=colq,y=dy)
  }
  return(res)
}

