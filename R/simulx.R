simulx <- function(model=NULL,group=NULL,treatment=NULL,parameter=NULL,output=NULL,
                   data=NULL, project=NULL, settings=NULL, regressor=NULL, varlevel=NULL)
{ 
  #--------------------------------------------------
  #  simulx.R is governed by the CeCILL-B license. 
  #  You can  use, modify and/ or redistribute the software under the terms of 
  #  the CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
  #  http://www.cecill.info/index.en.html
  #
  #  simulx.R was developed by Marc Lavielle (Inria, popix team) for the DDMoRe project. 
  #--------------------------------------------------
  
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  
  #--------------------------------------------------
  # Manage settings
  #--------------------------------------------------
  cc  <-  processing_setting(settings)
  s <- cc[[2]]
  data.in <- cc[[1]]
  
  
  if(is.null(data)){
    
    dataIn          <- list()    
    #--------------------------------------------------
    #    MODEL
    #--------------------------------------------------
    if (!(is.null(model))) {
      if(model=="pkmodel"){
        model = generateModelFromPkModel(parameter,output) 
      } else {
        model_ext <- file_ext(model)
        if(model_ext=="xml"){
          model = pharmml2mlxtran(model)
        }
      }
    }
    
    #--------------------------------------------------
    #     Monolix project
    #--------------------------------------------------
    if (!(is.null(project)))  {
      ans           <- processing_monolix(project,model,treatment,parameter,output,group)
      model         <- ans$model
      treatment     <- ans$treatment
      parameter     <- ans$param
      output        <- ans$output
      group         <- ans$group
    }
    
    #--------------------------------------------------
    lv <- list(treatment=treatment,
               parameter=parameter,
               output=output,
               regressor=regressor,
               varlevel=varlevel,
               group=group,
               model=model
    )
    lv  <- hformat(lv)
    
    dataIn    <-  hgdata(lv)
    dataIn$model <- model
    
  }else{
    dataIn=data
    group=NULL
  }
    
   if (length(s)==0){
     argList <- list(DATA=dataIn) 
   } else {
    argList <- list(DATA=dataIn, SETTINGS=s)       
   }
  
  dataOut  <- .Call( "mlxComputeR", argList)
  Sys.setenv(LIXOFT_HOME="")
  dataOut  <- convertmlx(dataOut,dataIn)
  
  if(is.null(data)){
    if(data.in==F){
      return(dataOut)
    }else{
      return(list(dataOut, dataIn))
    }
  }else{
    return(dataOut)
  }
}
