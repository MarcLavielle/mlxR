#' Convert a Monolix Project  into an executable for the simulator  Simulx 
#' @param project : the name of a Monolix project 
#' @param parameter : string $(NameOfTypeOfParameter), the type of specific parameters to use 
#'                   example: "mode", "mean"...
#' @param group : a list with the number of subjects 
#' @param open : load the R script created if \code{open=TRUE}
#' @param r.data : read the data if \code{r.data=TRUE}
#' @param add : add tables in the output folder
#' @return  creates a folder projectNameR  containing files : 
#' \itemize{
#'   \item \code{projectName.R} :  executable R code for the simulator,
#'   \item \code{treatment.txt} :  contains the treatment informations,
#'   \item \code{populationParameter.txt} : contains the  population parameters estimated from Monolix,
#'   \item \code{individualParameter.txt} : contains the  individual parameters (mode/mean) estimated from Monolix (if used for the simulation),
#'   \item \code{individualCovariate.txt} : contains the individual covariates,
#'   \item \code{originalId.txt} : contains the original id's when group is used with a different size than the original one,
#'   \item \code{outputi.txt} : contains the output number i informations (time, id),
#'   \item \code{$(NameOfTypeOfParameter)s.txt} : contains the specific parameter used.
#' }       
#'  
#' @return A list of data frames. Each data frame is an output of simulx the mlxtran model
#' the data inputs: treatment, parameters, output of monolix, group... 
#' 
#' @export

simrep <-function(project,parameter=NULL,group=NULL,output=NULL,open=FALSE,
                  r.data=TRUE,fim=NULL,npop=1,nrep=1,sep=" ",kw.max=100
)
{ 
  #------- project to be converted into Simulx project
  myOldENVPATH = Sys.getenv('PATH');
  initMlxLibrary()
  session=Sys.getenv("session.simulx")
  Sys.setenv(LIXOFT_HOME=session)
  if  (!is.null(names(group)))
    group <- list(group)
  ans <- processing_monolix(project=project,
                            model=NULL,
                            treatment=NULL,
                            parameter=parameter,
                            output=output,
                            group=group,
                            r.data=r.data,
                            fim=fim)
  model         <- ans$model
  treatment     <- ans$treatment
  parameter     <- ans$param
  output        <- ans$output
  group         <- ans$group
  regressor     <- ans$regressor
  fim           <- ans$fim
  infoParam     <- ans$infoParam
  mlxtranpath <- dirname(project)
  mlxtranfile = file_path_sans_ext(basename(project))
  mypath <- getwd()
  Rproject <- file.path(mypath,paste0(mlxtranfile,"_simulx"))
  if(file.exists(Rproject) )
    unlink(Rproject, recursive = TRUE, force = TRUE)
  modelname = basename(model)
  dir.create(Rproject, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  dir.create(Rproject, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  file.copy(model, Rproject, overwrite = FALSE)
  file.remove(model)
  model<-file.path(Rproject,modelname)
  
  #configure and write output 
  RprojectPath <- dirname(model)
  mlxtranfile = file_path_sans_ext(basename(project))
  projectExe <- file.path(RprojectPath,paste0(mlxtranfile,".R"))
  cat(paste0("# File generated automatically on ", Sys.time(),"\n \n"), file =projectExe, fill = FALSE, labels = NULL,append = TRUE)
  cat("library(mlxR)  \n \nsetwd(dirname(parent.frame(2)$ofile)) \n\n# model \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat(paste0("model<-\"",modelname,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  # write  treatment 
  if(!(is.null(treatment))){ 
    if (!is.null(treatment$value)){
      treat2<-matrix(treatment$value,nrow=nrow(treatment$value),ncol=ncol(treatment$value))
      colnames(treat2)<-treatment$colNames
      treatment <- treat2
    }
    write.table(treatment,file=file.path(Rproject,"/treatment.txt"),row.names=FALSE,quote=FALSE)
    cat("\n# treatment\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    cat("trt <- read.table(\"treatment.txt\", header = TRUE) \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  }
  
  # write  parameters   
  if(!(is.null(parameter))){  
    param.list <- NULL
    cat("\n# parameters \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    
    if (!is.null(ans$id)){
      outfile = file.path(Rproject,paste0("/originalId.txt"))      
      write.table(ans$id,file=outfile,row.names=FALSE,quote=FALSE)
      cat(paste0("originalId<- read.table('originalId.txt', header=TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
    }
    
    populationParameter <- parameter[[1]]
    if (!is.null(populationParameter)){
      outfile = file.path(Rproject,paste0("/populationParameter.txt"))      
      cat(paste0("populationParameter<- read.vector('populationParameter.txt') \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(populationParameter,file=outfile,col.names=FALSE,quote=FALSE)
      if (!is.null(param.list))
        param.list <- paste(param.list,"populationParameter",sep=",")  
      else
        param.list <- "populationParameter"
    } 
    
    individualCovariate <- parameter[[2]]
    if (!is.null(individualCovariate)){
      outfile = file.path(Rproject,paste0("/individualCovariate.txt"))      
      cat(paste0("individualCovariate<- read.table('individualCovariate.txt', header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(individualCovariate,file=outfile,row.names=FALSE,quote=FALSE)
      if (!is.null(param.list))
        param.list <- paste(param.list,"individualCovariate",sep=",")  
      else
        param.list <- "individualCovariate"
    } 
    individualParameter <- parameter[[3]]
    if (!is.null(individualParameter)){
      outfile = file.path(Rproject,paste0("/individualParameter.txt"))      
      cat(paste0("individualParameter<- read.table('individualParameter.txt', header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      write.table(individualParameter,file=outfile,row.names=FALSE,quote=FALSE)
      if (!is.null(param.list))
        param.list <- paste(param.list,"individualParameter",sep=",")  
      else
        param.list <- "individualParameter"
    } 
    
    param.list <- paste(param.list,sep=",")
    param.str <- paste0("list.param <- list(",param.list,")")
    cat(param.str, file =projectExe, fill = FALSE, labels = NULL, append = TRUE)   
  }
  
  # write f.i.m
  if(!(is.null(fim))) 
    write.table(fim$mat,file=file.path(Rproject,"/corrEstimate.txt"),row.names=FALSE,quote=FALSE) 
  
  # write  requested output 
  if(!(is.null(output)))
  {  
    cat("\n# output \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    
    if(length(output)==1)
    {
      # many types of output could exist
      cat(paste0("name<-\"",output[[1]]$name,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      cat(paste0("time<-read.table(\"output.txt\",header=TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      cat(paste0("out<-list(name=name,time=time) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      out2 <-matrix(output[[1]]$value,nrow=nrow(output[[1]]$value),ncol=ncol(output[[1]]$value))
      colnames(out2)<-output[[1]]$colNames
      outfile = file.path(Rproject,"/output.txt")
      write.table(out2,file=outfile,row.names=FALSE,quote=FALSE) 
      #cat("out<-list(out)\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    }else
    {    # many types of output could exist
      for(i in seq(1:length(output)))
      {
        cat(paste0("name<-\"",output[[i]]$name,"\"\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        if(!(is.null(output[[i]]$colNames)))
        {
          
          cat(paste0("time<-read.table(\"output",i,".txt\",header=TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          cat(paste0("out",i,"<-list(name=name,time=time) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
          
          out2 <-matrix(output[[i]]$value,nrow=nrow(output[[i]]$value),ncol=ncol(output[[i]]$value))
          colnames(out2)<-output[[i]]$colNames
          outfile = file.path(Rproject,paste0("/output",i))
          outfile = paste0(outfile,".txt")
          write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
        }else{
          cat(paste0("out",i,"<-list(name=name) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        }
      }
      
      cat("out<-list(out1", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
      for(i in seq(2,length(output)))
      {
        cat(paste0(",out",i), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)   
      }
      cat(")\n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    }
  }
  
  # regressor    
  if(!(is.null(regressor)))
  {  
    # re-order regressor if duplicated or unsorted data (by time) exists on an id.
    # assuming that the first column is id and the second column is time
    for(ir in seq(1:length(regressor)))
    {
      regvalue = regressor[[ir]]$value      
      #remove duplicated rows for the same id
      duprows <-c()
      for(i in 1: (nrow(regvalue)-1))
      {
        for(j in (i+1):nrow(regvalue))
        {
          
          if( (regvalue[i,1]==regvalue[j,1]) ) #same id
          {
            if(regvalue[i,2]==regvalue[j,2]) #duplicated time for the same id
            {
              duprows<-c(duprows,j)             
            }
          }else
          {
            #assuming that the regressor is already sorted by id
            break
          }
        }
      }
      if(!is.null(duprows))
      {
        regvalue<-regvalue[-duprows,]
      }
      #sort according to id and to time
      index<-order(regvalue[,1],regvalue[,2])
      regressor[[ir]]$value <-regvalue[index,]      
    }
    #write regresssor
    cat("\n# regressor \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
    # many types of output could exist
    nameOtherReg<-NULL
    if(length(regressor)>1)    
    {  
      cat(paste0("regressor<-c() \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
      
      for(i in seq(1:length(regressor)))
      {
        if(!(is.null(regressor[[i]]$name)))
        {
          namePi<-regressor[[i]]$name
        }else {
          namePi<-paste0("regressor",i)
        }
        
        nameOtherReg<-c(nameOtherReg,namePi)
        outfile = file.path(Rproject,paste0("/",namePi,".txt"))      
        cat(paste0(namePi,"<- read.table(\"",namePi,".txt\", header = TRUE) \n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
        cat(paste0("regressor<-c(regressor,",namePi,")\n"), file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
        out2<-NULL
        out2 <-matrix(regressor[[i]]$value,nrow=nrow(regressor[[i]]$value),ncol=ncol(regressor[[i]]$value))
        colnames(out2)< regressor[[i]]$colNames
        write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      }
    }else{
      outfile = file.path(Rproject,paste0("/regressor.txt"))      
      out2<-NULL
      out2 <-matrix(regressor[[1]]$value,nrow=nrow(regressor[[1]]$value),ncol=ncol(regressor[[1]]$value))
      colnames(out2)<-regressor[[1]]$colNames
      write.table(out2,file=outfile,row.names=FALSE,quote=FALSE)
      cat(paste0("regressor <-read.table(\"regressor.txt\", header = TRUE)\n"),file =projectExe, fill = FALSE, labels = NULL, append = TRUE)             
    }
  }
  # call the simulator
  cat("\n# call the simulator \n", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  cat("res <- simulx(model=model", file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  if(!(is.null(treatment)))
    cat(",treatment=trt",file =projectExe, fill = FALSE, labels = NULL, append = TRUE) 
  
  if(!(is.null(parameter)))
    cat(",parameter=list.param",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(group))) 
    cat(",group=grp",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(output)))
    cat(",output=out",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  if(!(is.null(regressor)))
    cat(",regressor=regressor",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  cat(")\n",file =projectExe, fill = FALSE, labels = NULL, append = TRUE)
  
  Sys.setenv('PATH'=myOldENVPATH);
  if( (Sys.getenv("RSTUDIO")=="1") & (open==TRUE) ) {
    file.edit(projectExe) 
    setwd(mypath)
  }
  
  ########
  ########
  param0 <- NULL
  if (!is.null(individualCovariate))
    param0 <- append(param0, list(individualCovariate))
  
  if (!is.null(individualParameter))
    param0 <- append(param0, list(individualParameter))
  
  pop.mat <- sim.pop(npop,populationParameter,infoParam,fim,kw.max)
  res <- NULL
  
  param.list <- append(param0,list(pop.mat[1,]))  
  r0 <- simulx(model=model,
               treatment=treatment, 
               output=output, 
               parameter=param.list, 
               group=group)
  nr <- length(r0)
  r.names <- names(r0)
  if (!is.null(r0$parameter)){
    p.names <- names(r0$parameter)
    f.names <- setdiff(r.names,p.names)
    r.ind <- which(is.element(r.names,f.names))
  } else {
    r.ind <- (1:length(r.names))
    f.names <- r.names
  }
  nr <- length(r.ind)
  for (k in (1:nr))
    f.names[k] <- file.path(Rproject,paste0(f.names[k],".txt"))
  
  dataIn <- simulx(model=model,
                   treatment=treatment, 
                   output=output, 
                   parameter=param.list, 
                   group=group,
                   settings=list(data.in=TRUE,load.design=FALSE))
  R.complete <- list()
  for (i.pop in (1:npop)) {
    print(i.pop)
    param.list <- append(param0,list(pop.mat[i.pop,]))  
    dataIn <- simulx(model=model,
                     treatment=treatment, 
                     output=output, 
                     parameter=param.list, 
                     group=group,
                     settings=list(data.in=TRUE,load.design=FALSE))
    for (i.rep in (1:nrep)) {
      r <- simulx(data=dataIn)
      r <- r[r.ind]
      if (i.rep==1) {
        for(k in (1:nr)){
          r[[k]]$rep <- i.rep
        }
        res <- r
      } else {
        for(k in (1:nr)){
          rk <- r[[k]]
          rk$rep <- i.rep
          res[[k]] <- rbind(res[[k]],rk)
        }
      }
    }
    R.complete[[i.pop]] <- res
    #     R.complete <- append(R.complete,res)
    for (k in (1:nr)){
      #       rk <- res[[k]]
      #       rk <- format(res[[k]], digits = 5, justify = "right", trim = FALSE)
      rk <- format(res[[k]], digits = 5, justify = "left")
      rk$pop <- i.pop
      n.col <- dim(rk)[2]
      rk<- rk[,c(n.col,n.col-1,(1:(n.col-2)))]
      if (nrep==1)
        rk$rep <- NULL
      if (npop==1)
        rk$pop <- NULL
      if (i.pop==1)
        write.table(rk,f.names[k],row.names=FALSE,quote=FALSE,sep=sep)
      else
        write.table(rk,f.names[k],append=TRUE,sep=sep,row.names=FALSE,col.names=FALSE,quote=FALSE)
    }
  }
  ##
  return(R.complete)
}


################################################"

sim.pop <- function(n,mu,infop,fim,kw.max){
  
  p1.name <- names(fim$se)
  np <- length(p1.name)
  p1.trans=rep("N",np)
  p2.name <- sub("_pop","",p1.name)
  #   iab <- c(which(infop$name=="a" | infop$name=="b"))
  #   infop$name=infop$name[-iab]
  #   infop$trans=infop$trans[-iab]
  i.pop <- match(infop$name,p2.name)
  i1 <- which(!is.na(i.pop))
  p1.trans[i.pop[i1]] <- infop$trans[i1]
  i.omega <- grep("omega_",p1.name)
  p1.trans[i.omega] <- "L"
  i.omega2 <- grep("omega2_",p1.name)
  p1.trans[i.omega2] <- "L"
  i.corr <- grep("r_",p1.name)
  p1.trans[i.corr] <- "R"
  #
  inan <- which(is.nan(as.matrix(fim$mat)),arr.ind=TRUE)
  if (dim(inan)[1]>0)
    i1 <- which(as.vector(table(inan[,1]))<np)
  else
    i1 <- (1:np)
  corr1 <- fim$mat[i1,i1]
  if (length(which(is.na(corr1)))>0)
    stop("ERROR:  the correlation matrix of the estimates contains NaN")
  se1 <- fim$se[i1]
  tr1 <- p1.trans[i1]
  mu1 <- mu[i1]
  #
  set <- se1
  mut <- mu1
  iL <- which(tr1=="L")
  set[iL] <- se1[iL]/mu1[iL]
  mut[iL] <- log(mu1[iL])
  iG <- which(tr1=="G")
  set[iG] <- se1[iG]/(mu1[iG]*(1-mu1[iG]))
  mut[iG] <- log(mu1[iG]/(1-mu1[iG]))
  iR <- which(tr1=="R")
  set[iR] <- se1[iR]*2/(1 - mu1[iR]^2)
  mut[iR] <- log((mu1[iR]+1)/(1-mu1[iR]))
  iP <- which(tr1=="P")
  set[iP] <- se1[iP]/dnorm(qnorm(mu1[iP]))
  mut[iP] <- qnorm(mu1[iP])
  Rt <- chol(corr1)*set
  K <- length(i1)
  #
  #   p.corr <-c("V","Cl","Imax","IC50","kin")
  #   n.r <- length(p.corr)
  #   R.r <- diag(rep(1,n.r))
  #   ir1 <- c(1,3,3,4)
  #   ir2 <- c(2,4,5,5)
  
  n.corr <- length(i.corr)
  if (n.corr>0){
    corr.name <- p1.name[i.corr]
    g=gregexpr("_",corr.name)
    nk1 <- vector(length=n.corr)
    nk2 <- vector(length=n.corr)
    for (k in (1:n.corr)){
      if (length(g[[k]])>2)
        stop('ERROR :  you should not use parameter names with "_" ')
      cnk <- corr.name[k]
      gk <- g[[k]][1:2]
      nk1[k] <- substr(cnk,gk[1]+1,gk[2]-1)
      nk2[k] <- substr(cnk,gk[2]+1,nchar(cnk))
    }
    nvar <- unique(c(nk1,nk2))
    ir1 <- match(nk1,nvar)
    ir2 <- match(nk2,nvar)
    ind1.r <- matrix(c(ir2,ir1),ncol=2)
    ind2.r <- matrix(c(ir1,ir2),ncol=2)
    n.r <- length(nvar)
    R.r <- diag(rep(1,n.r))
  }
  #
  n1 <- n
  s.res <- NULL
  kw <- 0
  while(n1 > 0){
    kw <- kw+1
    if (kw> kw.max)
      stop("Maximum number of iterations reached: could not draw definite positive correlation matrix")
    x=matrix(rnorm(K*n1),ncol=K)
    st <- t(t(x%*%Rt) + mut)
    st[,iL] <- exp(st[,iL])
    st[,iG] <- 1/(1+exp(-st[,iG]))
    st[,iP] <- pnorm(st[,iP])
    st[,iR] <- (exp(st[,iR])-1)/(exp(st[,iR])+1)
    s <- t(replicate(n1, mu))  
    s[,i1] <- st
    if (n.corr>0){
      for (i in (1:n1)){
        sri <- s[i,i.corr]
        R.r[ind1.r] <- sri
        R.r[ind2.r] <- sri
        R.eig <- eigen(R.r, symmetric=TRUE, only.values=TRUE)
        if (min(R.eig$values)>0){
          n1 <- n1-1
          s.res <- rbind(s.res,s[i,])
        }
      }
    } else {
      n1 <- 0
      s.res <- s
    }
    
  }
  return(s.res)
}

