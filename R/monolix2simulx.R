#' Convert a Monolix Project  into an executable for the simulator  Simulx 
#' @param projectName : the name of a Monolix project 
#' @return  creates a folder projectNameR  containing files : 
#' \itemize{
#'   \item \code{projectName.R} :  executable R code for the simulator,
#'   \item \code{treatment.txt} :  contains the treatment informations,
#'   \item \code{parameters.txt} : contains the  parameters ouput from Monolix,
#'   \item \code{group.txt} : contains the group informations
#'   \item \code{output.info} : contains the informations on the number of output of the mlxtran model
#'   \item \code{output_i.txt} : contains the output number i informations (time, id).
#' }       
#'  
#' @return A list of data frames. Each data frame is an output of simulxthe mlxtran model
#' the data inputs: treatment, parameters, output of monolix, group... 
#' 
#' @export
monolix2simulx <-function(project)
{
  #------- project to be converted into Simulx project
  
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  model=NULL
  group=NULL
  treatment=NULL
  parameter=NULL
  output=NULL
  
  ans           <- processing_monolix(project,model,treatment,parameter,output,group)
  model         <- ans$model
  treatment     <- ans$treatment
  parameter     <- ans$param
  output        <- ans$output
  group         <- ans$group
  mlxtranpath <- dirname(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  Rproject <- file.path(mlxtranpath,paste0(mlxtranfile,"R"))
  if(file.exists(Rproject) )
  {
    unlink(Rproject, recursive = TRUE, force = TRUE)
  }
  modelname = basename(model)
  dir.create(Rproject, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  file.copy(model, Rproject, overwrite = FALSE)
  file.remove(model)
  model<-file.path(Rproject,modelname)
  
  # write out parameters 
  if(!(is.null(parameter)))
  { 
    outinfo <-matrix(nrow=length(parameter),ncol=2)
    # many types of output could exist
    for(i in seq(1:length(parameter)))
    {      
      outfile = file.path(Rproject,paste0("/parameter_",i))
      outfile = paste0(outfile,".txt")
      if(!(is.null(parameter[[i]]$colNames)))
      {
        out2<-NULL
        out2 <-matrix(parameter[[i]]$value,nrow=nrow(parameter[[i]]$value),ncol=ncol(parameter[[i]]$value))
        outinfo[i,1]<-parameter[[i]]$name
        outinfo[i,2]<-parameter[[i]]$label
        colnames(out2)<-parameter[[i]]$colNames
        write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      } else{
        write.table(parameter[[i]],file=outfile,row.names=FALSE,quote=FALSE)
      }
      
       
    }
    write.table(outinfo,file=file.path(Rproject,"/parameter.info"),row.names=FALSE,quote=TRUE) 
  }
  # write out requested output 
  if(!(is.null(output)))
  { 
    outinfo <-matrix(nrow=length(output),ncol=2)
    # many types of output could exist
    for(i in seq(1:length(output)))
    {
      outinfo[i,1]<-output[[i]]$name
      outinfo[i,2]<-output[[i]]$label
      out2 <-matrix(output[[i]]$value,nrow=nrow(output[[i]]$value),ncol=ncol(output[[i]]$value))
      colnames(out2)<-output[[i]]$colNames
      outfile = file.path(Rproject,paste0("/output_",i))
      outfile = paste0(outfile,".txt")
      write.table(out2,file=outfile,row.names=FALSE,quote=FALSE) 
    }
    write.table(outinfo,file=file.path(Rproject,"/output.info"),row.names=FALSE,quote=TRUE) 
  }
  if(!(is.null(group)))
  { write.table(group,file=file.path(Rproject,"/group.txt"),row.names=FALSE,col.names=FALSE,quote=FALSE) }
  
  # write out  treatment 
  if(!(is.null(treatment)))
  { 
    #treatment$name = "doseRegmen"
    #treatment$label = "source"
    #treatment$colNames = colnames(outputFile)
    #treatment$value = values of outputFile
    treat2<-matrix(treatment$value,nrow=nrow(treatment$value),ncol=ncol(treatment$value))
    colnames(treat2)<-treatment$colNames
    write.table(treat2,file=file.path(Rproject,"/treatment.txt"),row.names=FALSE,quote=FALSE)
  }
  
  
  #-------  write executable R file
  #
  
  RprojectPath <- dirname(model)
  mlxtranfile = file_path_sans_ext(basename(project))
  projectExe <- file.path(RprojectPath,paste0(mlxtranfile,".R"))
  
  cat(paste0("# File generated automatically on ", Sys.time(),"\n \n"), file =projectExe, fill = FALSE, labels = NULL,append = TRUE)
  cat(paste0("model<-","\"",model,"\"","\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat(paste0("Rproject<- dirname(model)\n"), file =projectExe, fill = FALSE, labels = NULL,append = TRUE)
  message1 <- " \n# read files and get all the  settings"
  message2 <- " \n# call the simulator"
  message3 <- " \n# display the results"
  
  tmp<-deparse(body(simulatorAutoExeCode1))  
  # remove the first  and the last  braces delimiting the body of the function
  tmp<-tmp[2:(length(tmp)-1)]  
  write(tmp, projectExe, append = TRUE)
  write(message1, projectExe, append = TRUE)
  
  tmp<-deparse(body(simulatorAutoExeCode2))  
  # remove the first  and the last  braces delimiting the body of the function
  tmp<-tmp[2:(length(tmp)-1)]  
  write(tmp, projectExe, append = TRUE)
  write(message2, projectExe, append = TRUE)
  
  tmp<-deparse(body(simulatorAutoExeCode3))  
  # remove the first  and the last  braces delimiting the body of the function
  tmp<-tmp[2:(length(tmp)-1)]  
  write(tmp, projectExe, append = TRUE)
  write(message3, projectExe, append = TRUE)
  
  tmp<-deparse(body(simulatorAutoExeCode4))  
  # remove the first  and the last  braces delimiting the body of the function
  tmp<-tmp[2:(length(tmp)-1)]  
  write(tmp, projectExe, append = TRUE)
  
  # open the resulting code
  file.edit(projectExe)
  #execute the resulting code
  #source(projectExe)
}

simulatorAutoExeCode1<-function()
{ 
  
  library(gridExtra)
  treatment = NULL
  parameter = NULL
  output = NULL
  group = NULL  
  
}
simulatorAutoExeCode2<-function()
{ 
  filetmp =file.path(Rproject,"/treatment.txt")
  if(file.exists(filetmp))
  {
    treat2<- read.table(filetmp,header= TRUE)
    treatment$name = "doseRegimen"
    treatment$label = "source"
    treatment$colNames <- colnames(treat2)
    treatment$value <- as.matrix(treat2[,],nrow=nrow(treat2),ncol=ncol(treat2))
  }  
  
  filetmp = file.path(Rproject,"/parameter.info")
  
  if(file.exists(filetmp))
  {
    paraminfo<- read.table(filetmp,header=TRUE)
    parami<- NULL
    for(i in seq(1:nrow(paraminfo)))
    {    
      namei <- as.character(paraminfo[i,1])
      outfile = file.path(Rproject,paste0("/parameter_",i))
      outfile = paste0(outfile,".txt")
      value<-read.table(outfile,header=TRUE) 
      if(is.na(namei))
      {
        parami<-NULL        
        parami$name <-as.character(value[,1])
        parami$value<-value[,2]
        parameter <-c(parameter,list(parami))
      }else{
        parami<-NULL
        parami$name <- namei
        parami$label<- as.character(paraminfo[i,2])
        parami$colNames <-colnames(value)
        parami$value<-as.matrix(value[,],nrow=nrow(value),ncol=ncol(value))
        parameter <-c(parameter,list(parami))
      }
      
    }
    
  }
  
  filetmp = file.path(Rproject,"/output.info")
  if(file.exists(filetmp))
  {
    outinfo<- read.table(filetmp,header=TRUE)
    outi<- NULL
    for(i in seq(1:nrow(outinfo)))
    {    
      outi$name <- as.character(outinfo[i,1])
      outi$label<- as.character(outinfo[i,2])
      outfile = file.path(Rproject,paste0("/output_",i))
      outfile = paste0(outfile,".txt")
      value<-read.table(outfile,header=TRUE) 
      outi$colNames <-  colnames(value)
      outi$value<-as.matrix(value[,],nrow=nrow(value),ncol=ncol(value))
      output <-c(output,list(outi))
    }
  }
  
  filetmp =file.path(Rproject,"/group.txt")
  if(file.exists(filetmp))
  {
    group<- read.table(filetmp,header= TRUE)
  }
  
}
simulatorAutoExeCode3<-function()
{ 
  res <- simulx(model=model, treatment=treatment,parameter=parameter,
                output=output,group=group)
  
}
simulatorAutoExeCode4<-function()
{ 
  plotList<-NULL  
  for(i in seq(1:length(output)))
  {
    yname=output[[i]]$name
    plotList<-c(plotList, list(ggplot(data=res[[yname]])  +
                                 geom_point(aes_string(x="time", y=yname,colour="id")) +
                                 geom_line(aes_string(x="time", y=yname, colour="id")) + 
                                 scale_x_continuous("Time") + scale_y_continuous(as.character(yname))))
  }
  do.call(grid.arrange, c(plotList, ncol=floor(sqrt(length(output)))))
  
}