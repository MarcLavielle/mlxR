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