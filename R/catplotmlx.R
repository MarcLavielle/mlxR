#'  Plot Categorical Longitudinal Data
#'
#'  Plot the empirical distribution of categorical longitudinal data.
#'
#' See http://simulx.webpopix.org/mlxr/catplotmlx/ for more details.      
#' @param r a data frame with a column \samp{id}, a column \samp{time}, 
#' a column with values and possibly Hk[ja column \samp{group}.
#' @param breaks one of:
#' \itemize{
#'   \item a vector giving the breakpoints,
#'   \item a single number giving the number of segments.
#' }
#' @param color a color to be used for the plots (default="#194280")
#' @param group  variable to be used for defining groups (by default, \samp{group} is used when it exists)
#' @param facet  makes subplots for different groups if \code{TRUE} 
#' @param labels  vector of strings 
#' 
#' @return 
#' a ggplot object if \code{plot=TRUE} ; otherwise, a list with fields:
#' \itemize{
#'   \item color a vector of colors used for the plot 
#'   \item y a data frame with the values of the empirical distribution computed at each time point
#' }
#' @examples
#' \dontrun{
#'   catModel <- inlineModel("
#'   [LONGITUDINAL]
#'   input =  {a,b}
#'   EQUATION:
#'   lp1=a-b*t
#'   lp2=a-b*t/2
#'   DEFINITION:
#'   y = {type=categorical, categories={1,2,3}, 
#'   logit(P(y<=1))=lp1, logit(P(y<=2))=lp2}
#'   ")
#'   
#'   y.out  <- list(name='y', time=seq(0, 100, by=4))
#' 
#'   Ng  <- 1000
#'   g1 <- list(size=Ng, parameter=c(a=6,b=0.2))
#'   res <- simulx(model=catModel, output=y.out, group=g1)
#'   catplotmlx(res$y)
#'   catplotmlx(res$y, breaks=seq(-2,102,by=8), color="purple") 
#'   catplotmlx(res$y, breaks=5, color="#490917") 
#'   
#'   g2 <- list(size=Ng, parameter=c(a=10,b=0.2))
#'   res <- simulx(model=catModel, output=y.out, group=list(g1,g2))
#'   catplotmlx(res$y) 
#'   catplotmlx(res$y, group="none")
#'   
#'   g3 <- list(size=Ng, parameter=c(a=6,b=0.4))
#'   g4 <- list(size=Ng, parameter=c(a=10,b=0.4))
#'   res <- simulx(model=catModel, output=y.out, group=list(g1,g2,g3,g4))
#'   catplotmlx(res$y)
#'    
#'   cov <- data.frame(id=levels(res$y$id), a=rep(c(6,10,6,10),each=Ng), b=rep(c(0.2,0.2,0.4,0.4),each=Ng))
#'   catplotmlx(res$y, group=cov) 
#' }
#' @importFrom ggplot2 ggplot aes geom_polygon xlab ylab ylim ggtitle scale_fill_manual
#' @importFrom graphics hist
#' @importFrom grDevices hsv
#' @export         
catplotmlx <- function(r, col=NULL, breaks=NULL, plot=TRUE, color="#194280", 
                       group=NULL, facet=TRUE, labels=NULL)
{
  
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
  
  if (is.null(breaks)){
    t <- sort(unique(r[,col[2]]))
    nt <- length(t)
    zt <- c(t[1]-1,t,t[nt]+1)
    zt <- (zt[1:(nt+1)] + zt[2:(nt+2)])/2
  }else{
    if (length(breaks)>1){
      zt <- breaks
      nt <- length(zt)-1
    }else{
      nt  <- breaks
      zt1 <- min(r[,col[2]])
      zt2 <- max(r[,col[2]])
      dzt <- (zt2-zt1)/(10*nt)
      zt  <- seq(zt1-dzt,zt2+dzt,length.out=(nt+1))
    }
    t <- (zt[1:(nt)] + zt[2:(nt+1)])/2
  }
  
  r.name <- names(r)[col[3]]
  names(r)[col[3]] <- "y"
  r$y<- factor(r$y)
  y.levels <- levels(r[,col[3]])
  y.nlevels <- nlevels(r[,col[3]])
  # z <- c(y[1]-1,y,y[y.nlevels]+1)
  # br <- (z[1:(y.nlevels+1)] + z[2:(y.nlevels+2)])/2
  v <- rep(y.levels, each=2*nt)
  x <- rep(c(t,rev(t)),y.nlevels)
  
  sfm <- list()
  col.hsv <- rgb2hsv(col2rgb(color))
  color=hsv(col.hsv[1],col.hsv[2],col.hsv[3],seq(0.3,0.9,length.out=y.nlevels))
  sfm = scale_fill_manual(name=r.name,values=color)
  
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
    ig <- factor(rep(1, n.tot))
  }
  
  ug <- levels(ig)
  ng <- length(ug)
  N <- nlevels(r$id)
  
  y <- list()
  for (k in (1:ng)) {
    jk <- which(ig==ug[k])
    rk<-r[jk,col[3]]
    H <- matrix(nrow=nt,ncol=y.nlevels)
    for (j in (1:nt)){
      yj <- rk[which(r$time>zt[j] & r$time<zt[j+1] )]
      hj <- table(yj)
      H[j,] <- hj/sum(hj)
    }
    H <- cbind(0,H)
    y[[k]] <- apply(H, 1, cumsum)
  }
  
  if (plot==TRUE) {
    datapoly <- NULL
    pk <- ggplotmlx()
    for (k in (1:ng)) {
      Hk <- y[[k]]
      pr <- NULL
      for (j in (1:y.nlevels))
        pr <- c(pr,Hk[j,],rev(Hk[j+1,]))
      dk <- data.frame(x,pr,v)  
      if (!is.null(group)) {
        jk <- which(ig==ug[k])
        dk[group] <- r[group][jk[1],]
      }
      datapoly <- rbind( datapoly, dk)
      pk <- pk + geom_polygon(data=dk, aes(x=x, y=pr,fill=v, group=v)) 
    }
    
    pk <- pk  +  xlab("time")+ylab("probability")+ylim(c(0,1)) + sfm
    # pk <- ggplotmlx(datapoly) + geom_polygon(aes(x=x, y=pr,fill=v, group=v)) +
    #   xlab("time")+ylab("probability")+ylim(c(0,1)) +sfm
     
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
      if (!is.null(group)) {
        tyk[group] <- r[group][jk[1],]
        jk <- which(ig==ug[k])
        }
      dy=rbind(dy, tyk)
    }
    names(dy)=c("time", "baseline",y.levels)
    res <- list(color=color,y=dy)
  }
  return(res)
}  


