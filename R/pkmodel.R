#' Easy simulation of basic PK models
#'        
#' @export
#' @param time a list
#' @param treatment a list with two fields
#' \itemize{
#'   \item \code{number} the number of intervals (i.e. the number of percentiles minus 1).
#'   \item \code{level} level the largest interval (i.e. the difference between the lowest and the highest percentile). 
#' }
#' @param parameter vector of parameters with their names
#' @param dose ...
#' 
#' @examples
#' adm <- list(time=c(2,14,20), amount=40)
#' p   <- c(V=8, Cl=0.5,k12=0.3, k21=0.2)
#' t   <- seq(0, 30, by=0.1)
#' 
#' res   <- pkmodel(time = t, treatment = adm, parameter = p)
#' 
#' print(ggplot(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
#'   xlab("time (h)") + ylab("concentration (mg/L)"))
#' 
#' adm <- list(time = c(1,23,37,45), amount = c(1,0.5,2,0.3))
#' p <- c(Mtt=5, Ktr=1, ka=0.5, V=10, Vm=1, Km=0.6, p=0.5)
#' t <- seq(0, 80, by=0.1)
#' 
#' res <- pkmodel(t,adm,p)
#' 
#' print(ggplot(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
#'   xlab("time (h)") + ylab("concentration (mg/L)"))
#' 
#' adm <- list( time = 2, amount = 40)
#' 
#' p <- inlineDataFrame("
#' id   ka   V    Cl
#' 1   0.5   4     1
#' 2     1   6     1
#' 3   1.5   6   1.5
#' ")
#' 
#' t <- seq(0, 30, by=0.1)
#' 
#' res <- pkmodel(t,adm,p)
#' 
#' print(ggplot(data=res, aes(x=time, y=cc, colour=id)) + geom_line(size=1) +
#'   xlab("time (h)") + ylab("concentration (mg/L)"))   
#' adm <- list(time=seq(2, 100, by=24), amount=40, rate=5)
#' p <- c(V=8, Cl=0.5, k12=0.3, k21=0.2, ke0=0.2)
#' t <- seq(0, 50, by=0.1)
#' 
#' res <- pkmodel(t,adm,p)
#' 
#' if( require("reshape2") ){
#'   r <- melt(res, id='time', variable.name='c')
#'   print(ggplot(r, aes(time,value)) + geom_line(aes(colour = c),size=1) +
#'           ylab('concentration') + guides(colour=guide_legend(title=NULL)) +
#'           theme(legend.position=c(.9, .8)))
#' }
pkmodel <- function(time,treatment,parameter,dose=NULL){
  # ########################################################################################  
  #  pkmodel.R is governed by the CeCILL-B license. 
  #  You can  use, modify and/ or redistribute the software under the terms of 
  #  the CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
  #  http://www.cecill.info/index.en.html
  #
  #  pkmodel.R was developed by Marc Lavielle and Fazia Bellal (Inria) for the DDMoRe project. 
  # ########################################################################################  
  if (!is.null(dose))
    treatment <- dose
  
  if (!is.list(parameter)){
    parameter <- list(name=names(parameter), value=as.numeric(parameter))
  }
  if (is.data.frame(parameter)){
    np <- names(parameter)
    jid <- which(np=="id")
    parameter <- list(name=np[-jid], colNames=np, value=as.matrix(parameter))
  }
     pn=parameter$name
  
#   pn=names(parameter)
  if(length(grep("ke0",pn))>0){iop.ke0=1}else{iop.ke0=0}
  if(iop.ke0==0){
    out <- list(name="cc",time=time)
  }
  else{
    out <- list(name=c("cc","ce"),time=time)
  }
  
  data <- simulx(model="pkmodel",parameter=parameter,output=out,treatment=treatment)
  if(iop.ke0==0){
    r=data$cc
  }
  else{
    r=merge(data$cc,data$ce)
  }
  
  if (isfield(r,"id")){
  if (length(levels(r[,"id"]))==1)
    r[,"id"]=NULL
  }
  
  return(r)
}