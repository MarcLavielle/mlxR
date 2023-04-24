#' Easy simulation of PK models
#'        
#' Easy simulation of basic PK models
#'        
#' See https://simulx.lixoft.com/mlxr-documentation/ for more details.
#' @export
#' @param time a vector
#' @param treatment a list with fields
#' \itemize{
#'   \item \code{time} : a vector of input times,
#'   \item \code{amount} : a scalar or a vector of amounts,
#'   \item \code{rate} : a scalar or a vector of infusion rates (default=\code{Inf}),
#'   \item \code{tinf} : a scalar or a vector of infusion times (default=0),
#' }
#' @param parameter vector of parameters with their names and values
#' 
#' @examples    
#' \dontrun{
#'   adm <- list(time=c(2,14,20), amount=40)
#'   p   <- c(V=8, Cl=0.5,k12=0.3, k21=0.2)
#'   t   <- seq(0, 30, by=0.1)
#'   
#'   res   <- pkmodel(time = t, treatment = adm, parameter = p)
#'   
#'   print(ggplot(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
#'     xlab("time (h)") + ylab("concentration (mg/L)"))
#'   
#'   adm <- list(time = c(1,23,37,45), amount = c(1,0.5,2,0.3))
#'   p <- c(Mtt=5, Ktr=1, ka=0.5, V=10, Vm=1, Km=0.6, p=0.5)
#'   t <- seq(0, 80, by=0.1)
#'   
#'   res <- pkmodel(t,adm,p)
#'   
#'   print(ggplot(data=res, aes(x=time, y=cc)) + geom_line(size=1) +
#'     xlab("time (h)") + ylab("concentration (mg/L)"))
#'   
#'   adm <- list( time = 2, amount = 40)
#'   
#'   p <- inlineDataFrame("
#'   id   ka   V    Cl
#'   1   0.5   4     1
#'   2     1   6     1
#'   3   1.5   6   1.5
#'   ")
#'   
#'   t <- seq(0, 30, by=0.1)
#'   
#'   res <- pkmodel(t,adm,p)
#'   
#'   print(ggplot(data=res, aes(x=time, y=cc, colour=id)) + geom_line(size=1) +
#'     xlab("time (h)") + ylab("concentration (mg/L)"))   
#'   adm <- list(time=seq(2, 100, by=24), amount=40, rate=5)
#'   p <- c(V=8, Cl=0.5, k12=0.3, k21=0.2, ke0=0.2)
#'   t <- seq(0, 50, by=0.1)
#'   
#'   res <- pkmodel(t,adm,p)
#'   
#'   if( require("reshape2") ){
#'     r <- melt(res, id='time', variable.name='c')
#'     print(ggplot(r, aes(time,value)) + geom_line(aes(colour = c),size=1) +
#'             ylab('concentration') + guides(colour=guide_legend(title=NULL)) +
#'             theme(legend.position=c(.9, .8)))
#'   } 
#' }
pkmodel <- function(time,treatment,parameter){
  # ########################################################################################  
  #  pkmodel.R is governed by the CeCILL-B license. 
  #  You can  use, modify and/ or redistribute the software under the terms of 
  #  the CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
  #  http://www.cecill.info/index.en.html
  #
  #  pkmodel.R was developed by Marc Lavielle and Fazia Bellal (Inria) for the DDMoRe project. 
  # ########################################################################################  
  
  pn <- names(parameter)
  
  if("ke0" %in% pn) {
    out <- list(name=c("cc","ce"),time=time)
  } else {
    out <- list(name="cc",time=time)
  }
  
  pn[which(pn=="V1")]="V"
  pn[which(pn=="Q")]="Q2"
  names(parameter) <- pn
  
  if ("V2" %in% pn){
    parameter["V2"] <- parameter["Q2"]/parameter["V2"]
    parameter["Q2"] <- parameter["Q2"]/parameter["V"]
    pn[which(pn=="V2")]="k21"
    pn[which(pn=="Q2")]="k12"
  }
  
  if ("V3" %in% pn){
    parameter["V3"] <- parameter["Q3"]/parameter["V3"]
    parameter["Q3"] <- parameter["Q3"]/parameter["V"]
    pn[which(pn=="V3")]="k31"
    pn[which(pn=="Q3")]="k13"
  }
  
  names(parameter) <- pn
  pk.model <- generateModelFromPkModel(parameter,out) 
  data <- simulx(model=pk.model, parameter=parameter, output=out, treatment=treatment)
  
  if("ke0" %in% pn) {
    r=merge(data$cc,data$ce)
  } else {
    r=data$cc
  }
  
  if (!is.null(r$id)){
    if (length(levels(r[,"id"]))==1)
      r[,"id"]=NULL
  }
  
  return(r)
}
