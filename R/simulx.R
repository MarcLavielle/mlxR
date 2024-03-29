#' Simulation of mixed effects models and longitudinal data
#'
#' Compute predictions and sample data from \code{Mlxtran} and \code{R} models
#' 
#' simulx takes advantage of the modularity of hierarchical models for simulating 
#' different components of a model: models for population parameters, individual 
#' covariates, individual parameters and longitudinal data.
#' 
#' Furthermore, \code{simulx} allows to draw different types of longitudinal data, 
#' including continuous, count, categorical, and time-to-event data.
#' 
#' The models are encoded using either the model coding language \samp{Mlxtran}, or \samp{R}. 
#' \samp{Mlxtran} models are automatically converted into C++ codes, 
#' compiled on the fly and linked to R using the \samp{Rcpp} package. 
#' That allows one to implement very easily complex models and to take advantage 
#' of the numerical sovers used by the C++ \samp{mlxLibrary}.
#' 
#' See https://simulx.lixoft.com/mlxr-documentation/ for more details.      
#' @param model a \code{Mlxtran}, or \code{R} model used for the simulation
#' @param parameter a vector of parameters with their names and values
#' @param output a list (or list of lists) with fields: 
#' \itemize{
#'   \item \code{name}: a vector of output names
#'   \item \code{time}: a vector of times (only for the longitudinal outputs)
#'   \item \code{lloq}: lower limit of quantification (only for the longitudinal outputs)
#'   \item \code{uloq}: upper limit of quantification (only for the longitudinal outputs)
#'   \item \code{limit}: lower bound of the censoring interval (only for the longitudinal outputs)
#' }
#' @param treatment a list with fields
#' \itemize{
#'   \item \code{time} : a vector of input times,
#'   \item \code{amount} : a scalar or a vector of amounts,
#'   \item \code{rate} : a scalar or a vector of infusion rates (default=\code{Inf}),
#'   \item \code{tinf} : a scalar or a vector of infusion times (default=0),
#'   \item \code{type} : the type of input (default=1),
#'   \item \code{target} : the target compartment (default=NULL). 
#' }
#' @param regressor a list, or a list of lists, with fields
#' \itemize{
#'   \item \code{name} : a vector of regressor names,
#'   \item \code{time} : a vector of times,
#'   \item \code{value} : a vector of values.
#' }
#' @param varlevel (IOV supported by mlxR >= 3.2.2) a list (or a dataframe) with fields: 
#' \itemize{
#'   \item \code{name} : name of the variable which defines the occasions,
#'   \item \code{time} : a vector of times (beginnings of occasions) ,
#' }
#' @param group a list, or a list of lists, with fields: 
#' \itemize{
#'   \item \code{size} : size of the group (default=1),
#'   \item \code{level} : level(s) of randomization,
#'   \item \code{parameter} : if different parameters per group are defined,
#'   \item \code{output} : if different outputs per group are defined,
#'   \item \code{treatment} : if different treatements per group are defined,
#'   \item \code{regressor} : if different regression variables per group are defined.
#' }
#' @param addlines a list with fields: 
#' \itemize{
#'   \item \code{section}: a string (default = "[LONGITUDINAL]"),
#'   \item \code{block}: a string (default = "EQUATION:"),
#'   \item \code{formula}: string, or vector of strings, to be inserted .
#' }
#' @param data a list (output of simulx when settings$data.in==TRUE)
#' @param project the name of a Monolix project
#' @param nrep number of replicates
#' @param npop number of population parameters to draw randomly 
#' @param fim a string with the Fisher Information Matrix to be used 
#' @param result.folder the name of the folder where the outputs of simulx should be stored
#' @param result.file the name of the single file where the outputs of simulx should be saved
#' @param stat.f a R function for computing some summary (mean, quantiles, survival,...) of the simulated data. Default = "statmlx".
#' @param settings a list of optional settings
#' \itemize{
#'   \item \code{seed} : initialization of the random number generator (integer),
#'   \item \code{load.design} : TRUE/FALSE (if load.design is not defined, a test is automatically performed to check if a new design has been defined),
#'   \item \code{data.in} : TRUE/FALSE (default=FALSE)
#'   \item \code{id.out}  : add columns id (when N=1) and group (when #group=1), TRUE/FALSE (default=FALSE)
#'   \item \code{kw.max} : maximum number of trials for generating a positive definite covariance matrix (default = 100) 
#'   \item \code{sep} : the field separator character (default = ",") 
#'   \item \code{digits} : number of decimal digits in output files (default = 5) 
#'   \item \code{disp.iter} : TRUE/FALSE (default = FALSE) display replicate and population numbers
#'   \item \code{replacement} : TRUE/FALSE (default = FALSE) sample id's with/without replacement
#'   \item \code{out.trt} : TRUE/FALSE (default = TRUE) output of simulx includes treatment
#'   \item \code{format.original} : TRUE/FALSE (default = FALSE) with a Monolix project, write data in result.file using the original format of the data file
#' }       
#' 
#' @return A list of data frames. Each data frame is an output of simulx
#' 
#' @examples
#' \dontrun{
#' myModel <- inlineModel("
#' [LONGITUDINAL]
#' input = {A, k, c, a}
#' EQUATION:
#' t0    = 0 
#' f_0   = A
#' ddt_f = -k*f/(c+f)
#' DEFINITION:
#' y = {distribution=normal, prediction=f, sd=a}
#' [INDIVIDUAL]
#' input = {k_pop, omega}
#' DEFINITION:
#' k = {distribution=lognormal, prediction=k_pop, sd=omega}
#' ")
#' f <- list(name='f', time=seq(0, 30, by=0.1))
#' y <- list(name='y', time=seq(0, 30, by=2))
#' res <- simulx(model     = 'model/home.txt', 
#'               parameter = c(A=100, k_pop=6, omega=0.3, c=10, a=2), 
#'               output    = list(f,y,"k"),
#'               group     = list(size=4, level='individual'))
#' 
#' plot(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour=id)) +
#'      geom_point(data=res$y, aes(x=time, y=y, colour=id)))
#' print(res$parameter)
#' }
#' 
#' @importFrom stats runif
#' @export

simulx <- function(model=NULL, parameter=NULL, output=NULL,treatment=NULL, 
                   regressor=NULL, varlevel=NULL, group=NULL, 
                   data=NULL, project=NULL, nrep=1, npop=NULL, fim=NULL, 
                   result.folder=NULL, result.file=NULL, stat.f="statmlx",
                   addlines=NULL, settings=NULL)
{ 
  #--------------------------------------------------
  #  simulx.R is governed by the CeCILL-B license. 
  #  You can  use, modify and/ or redistribute the software under the terms of 
  #  the CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
  #  http://www.cecill.info/index.en.html
  #
  #  simulx.R was developed by Marc Lavielle and the Inria popix and Xpop teams. 
  #--------------------------------------------------
  
  # !! RETRO-COMPTATIBILITY ========================================================== !!
  if (!.useLixoftConnectors()) # < 2019R1
    myOldENVPATH = Sys.getenv('PATH')
  else if (!.checkLixoftConnectorsAvailibility()) # >= 2019R1
    return()
  # !! =============================================================================== !!
  
  if (!initMlxR()$status)
    return()
  
  # !! RETRO-COMPTATIBILITY ========================================================== !!
  useLixoftConnectors <- .useLixoftConnectors()
  
  if (!useLixoftConnectors){ # < 2019R1
    session = Sys.getenv("session.simulx")
    Sys.setenv(LIXOFT_HOME = session)
  }
  # !! =============================================================================== !!  
  
  
  r <- simulx.check(model=model,parameter=parameter,output=output,treatment=treatment,regressor=regressor, 
                    varlevel=varlevel, group=group, data=data,project=project,settings=settings)
  
  for (r.field in names(r)) {eval(parse(text=paste0(r.field,"=r$",r.field)))}
  
  set.seed(settings$seed)
  disp.iter <- ifelse((!is.null(settings$disp.iter) && settings$disp.iter==TRUE), TRUE, FALSE)
  sep <- settings$sep
  digits <- settings$digits
  kw.max <- settings$kw.max
  replacement <- settings$replacement
  out.trt <- settings$out.trt
  
  remove.model <- remove.model0 <- FALSE
  
  if (!is.null(data)) {
    imodel.inline <- FALSE
    if (is.list(data$model)) {
      write(data$model$str, data$model$filename)
      data$model <- data$model$filename
      imodel.inline <- TRUE
    }
    if (!is.null(data$individual_parameters))
      data$individual_parameters$value <- matrix(data$individual_parameters$value, nrow=nrow(data$individual_parameters$value))
    r <- simulxunit(data=data,settings=settings,riov=NULL)
    if (imodel.inline==TRUE)
      file.remove(data$model)
    
    # !! RETRO-COMPTATIBILITY ======================================================== !!
    if (!useLixoftConnectors){ # < 2019R1
      Sys.setenv(LIXOFT_HOME = "")
      Sys.setenv('PATH' = myOldENVPATH); 
    }
    # !! ============================================================================= !!    
    
    return(r)
    
  }
  
  # if (any(sapply(parameter,is.character))) {
  #   iop_indiv=1
  # } else {
  #   iop_indiv=0
  # }
  
  
  #--------------------------------------------------
  #    MODEL
  #--------------------------------------------------
  
  # inline model
  imodel.inline <- FALSE
  if (is.list(model)) {
    imodel.inline <- TRUE
    if (imodel.inline==TRUE) {
      write(model$str, model$filename)
      model <- model$filename
      remove.model <- TRUE
      remove.model0 <- TRUE
    }
  }
  
  #--------------------------------------------------
  # R MODEL
  Rmodel <- FALSE
  if (identical(file_ext(model),"R")) {
    Rmodel <- TRUE
  } else {
    if ( !is.null(model) && !is.list(model) && exists(model, mode="function") )
      Rmodel <- TRUE
  }
  
  #--------------------------------------------------
  #     reshape inputs
  group     <- mklist(group)
  parameter <- mklist(parameter, add.name = FALSE)
  treatment <- mklist(treatment)
  regressor <- mklist(regressor)
  varlevel  <- mklist(varlevel)
  output    <- mklist(output)
  
  
  #--------------------------------------------------
  #     stat output
  #--------------------------------------------------
  if (!is.null(result.folder) || !is.null(result.file))
    write.simul=TRUE
  else
    write.simul=FALSE
  
  stat.n <- NULL
  stat.0 <- NULL
  stat.a <- list()
  loq.n <- NULL
  loq.a <- list()
  loq.arg <- c("limit", "lloq", "uloq")
  arg.names <- c("name", "time", loq.arg)

  if (length(output)>0) {
    ik0 <- NULL
    for (k in (1:length(output))){
      outk <- output[[k]]
      if (is.null(outk$name))
        stop("\n'name' is missing in the definition of an output\n", call.=FALSE)
      outk$type <- NULL
      if (!all(sapply(outk[loq.arg],"is.null"))) {
        loq.n <- c(loq.n, outk$name)
        iloq <- which(sapply(outk[loq.arg],"is.null")==FALSE)
        loq.a <- c(loq.a, rep(list(outk[loq.arg[iloq]]),length(outk$name)))
        if (!(is.null(project)) && is.null(outk$time))  
          ik0 <- c(ik0, k)
      }
      if (is.null(outk$time))
        outk.n <- "parameter"
      else
        outk.n <- outk$name
      outk[arg.names] <- NULL
      if (length(outk)>0) {
        stat.n <- c(stat.n, outk.n)
        stat.a <- c(stat.a, rep(list(outk),length(outk.n)))
      } else if (write.simul==TRUE) {
        stat.0 <- c(stat.0, outk.n)
      }
    }
    output[ik0] <- NULL
    if (length(output)==0)
      output <- NULL
  }
  if (write.simul==TRUE)
    stat.0 <- c(stat.0, "treatment", "covariate")
  names(stat.a)=stat.n
  names(loq.a)=loq.n
  
  iop_indiv <- 0
  #--------------------------------------------------
  #     Monolix project
  #--------------------------------------------------
  if (!(is.null(project))) {
    if (!is.null(npop)) {
      iproj.pop <- TRUE
      if (is.null(fim))  fim <- "needed"
    } else
      iproj.pop <- FALSE
    
    if (is.list(parameter[[1]]) && !is.null(parameter[[1]]$pop))
      p.pop <- parameter[[1]]
    else
      p.pop <- NULL
    
    ans <- processing_monolix( project=project,
                               treatment=treatment,
                               parameter=parameter,
                               regressor=regressor,
                               output=output,
                               group=group,
                               fim=fim,
                               format.original=settings$format.original)
    ans <- select.data(ans)
    model     <- ans$model
    treatment <- ans$treatment
    parameter <- ans$param
    output    <- ans$output
    group     <- ans$group
    regressor <- ans$regressor
    varlevel  <- ans$occasion
    fim       <- ans$fim
    infoParam <- ans$infoParam
    iop_indiv <- ans$iop_indiv
    format.original  <- ans$format.original
    id        <- as.factor(ans$id$oriId)
    N         <- nlevels(id)
    
    test.pop <- FALSE
    if (iproj.pop==TRUE) {
      test.pop <- TRUE
      if(is.null(fim))
        stop('The variance-covariance matrix of the estimates is requested for simulating several replicates of the population parameters. 
  Select "Standard errors" in the list of tasks.', call.=FALSE)
      else 
        parameter[[1]] <- sim.pop(npop,parameter[[1]],infoParam,fim,kw.max=kw.max)
      parameter[[1]]$pop <- (1:npop)
    } else if (!is.null(p.pop)) {
      parameter[[1]] <- p.pop
      iproj.pop <- TRUE
    }
    test.project <- TRUE
    test.N <- TRUE
  } else {
    test.project <- FALSE
    #--------------------------------------
    
    l.input <- c('parameter', 'treatment', 'regressor', 'varlevel', 'output')
    popid <- list()
    N <- NULL
    id <- NULL
    test.N <- FALSE
    test.pop <- FALSE
    if (!is.null(group)) {
      fy <- function(y) any(unlist(lapply(y, function(x) {'id' %in% names(x)})))
      if (any(unlist(lapply(group, fy))))  test.N <- TRUE
    }
    for (k in (1:length(l.input))) {
      lk <- l.input[k]
      eval(parse(text=paste0('pk <- ',lk))) 
      pk<- dpopid(pk,lk)
      if (!is.null(pk$N)) {
        test.N <- TRUE
        if (!is.null(pk$id))
          id <- unique(c(id, pk$id))
      }
      if (!is.null(pk$npop)) {
        test.pop <- TRUE
        npop <- unique(c(npop,pk$npop))
      }
      popid[[k]] <- pk
    }
    if (!is.null(id)) {
      l.input <- c('parameter', 'regressor', 'varlevel')
      for (k in (1:length(l.input))) {
        lk <- l.input[k]
        eval(parse(text=paste0('pk <- ',lk))) 
        pk<- dpopid(pk,lk)
        if (!is.null(pk$id) & !identical(id, pk$id))
          stop(paste0("Some id's are missing in '",lk,"'"), call.=FALSE)
      }
    }
    if (test.N==TRUE)
      id <- as.factor(id)
    N <- length(id)
  }
  
  if (!Rmodel ) {
    r <- commentModel(model, parameter, test.project)
    model <- r$model
    test.project <- r$test.project
  }
  
  #--------------------------------------------------
  #     variability levels
  #--------------------------------------------------
  if (!is.null(varlevel)) {
    n.varlevel <- length(varlevel)
    names.varlevel <- nocc <- vector(length = n.varlevel)
    for (k in (1:n.varlevel)) {
      vk <- varlevel[[k]]
      if (is.data.frame(vk)) {
        names.varlevel[k]  <- setdiff(names(vk), c("id","time"))[1]
        nocc[k] <- max(varlevel[[k]][names.varlevel[k]])
      } else {
        names.varlevel[k]  <- vk$name
        nocc[k] <- length(varlevel[[k]]$time)
        varlevel[[k]]$value <- 1:nocc[k]
      }
    }
    test.iov <- TRUE
    parameter <- parameter[sapply(parameter,length)>0]
    r <- param.iov(parameter, varlevel)
    parameter <- r$param
    if (iop_indiv==0) {
      riov <- translateIOV(model, names.varlevel, nocc, output, r$iov, r$cat)
      if (test.project)
        file.remove(model)
      model <- riov$model
      output <- outiov(output,riov$iov,varlevel,r$iov)
    } else {
      riov <- translateIOVind(model, names.varlevel, nocc, r$iov)
      file.remove(model)
      model <- riov$model
      output <- outiov(output,r$iov,varlevel,r$iov)
      # riov <- NULL
    }
    regressor <- c(regressor, varlevel)
    varlevel <- NULL
  } else 
    riov <- NULL
  #--------------------------------------------------
  #     Pop parameters
  #--------------------------------------------------
  if (test.pop == TRUE) {
    for (k in (1: length(parameter))) {
      paramk <- parameter[[k]]
      if (isfield(paramk,"pop")) {
        if (isfield(paramk,"id"))
          stop("Both 'id' and 'pop' cannot be defined in the same data frame", call.=FALSE)
        npop <- nrow(paramk)
        paramk$pop <- NULL
        parameter[[k]] <- paramk[1,]
        if (npop>=1) {
          test.pop <- TRUE
          k.pop <- k
          pop.mat <- paramk
        }
      }
    } 
  }
  
  #--------------------------------------------------
  #     Add equations in the Mlxtran model code
  #--------------------------------------------------
  if (!is.null(addlines))
    model <- modify.mlxtran(model, addlines)
  
  # For time to event output, add a right censoring time = 1e10 if missing
  # remove level=id from correlation definitions
  if (!Rmodel) {
    model0 <- model
    model <- rct.mlxtran(model)
    if (!identical(model, model0))
      remove.model <- TRUE
  }
  #--------------------------------------------------
  lv <- list(treatment=treatment,
             parameter=parameter,
             output=output,
             regressor=regressor,
             varlevel=varlevel,
             id=id)
  if (test.N==TRUE && !is.null(group)) {
    if (any(sapply(group, function(x) is.null(x$size))))
      stop("'size' is missing in group", call.=FALSE)
    g.size <- sapply(group, function(x) x$size)
    if (any(sapply(group, function(x) !is.null(x$level))))
      warning("'level' in group is ignored when id's are defined in the inputs of simulx", call.=FALSE)
    ng <- length(group)
    if (ng==1) {
      if (!identical(names(group[[1]]),'size'))
        stop("Only 'size' can be defined in group when a single group is created and when id's are defined in the inputs of simulx", call.=FALSE)
    } else {
      u.name <- unique(unlist(sapply(group, function(x) names(x))))
      if (!all(u.name %in% c("size","treatment", "regressor", "level")))
        stop("Only 'size', 'treatment' and 'regressor' can be defined in group when several groups are created and when id's are defined in the inputs of simulx", call.=FALSE)
      if ("treatment" %in% u.name) {
        tr <- NULL
        i2 <- 0
        for (k in (1:ng)) {
          i1 <- i2+1
          i2 <- i2 + group[[k]]$size
          tk <- as.data.frame(group[[k]]$treatment)
          ntk <- nrow(tk)
          tk <- tk[rep(seq.int(1,ntk), group[[k]]$size), ]
          tk$id <- rep(seq(i1,i2),each=ntk)
          if (is.null(tr))
            tr <- tk
          else
            tr <- merge(tr,tk, all=TRUE, sort=FALSE)
        }
        tr[is.na(tr)]='.'
        lv$treatment <- tr
      }
      if ("regressor" %in% u.name) {
        # gr1 <- group[[1]]$regressor
        # if (!is.null(names(gr1))) {
        #   for (k in (1:ng)) {
        #     group[[k]]$regressor <- list(group[[k]]$regressor)
        #   }
        # }
        # nreg <- length(group[[1]]$regressor)
        # lv$regressor <- list()
        # for (jr in 1:nreg) {
        tr <- NULL
        i2 <- 0
        for (k in (1:ng)) {
          i1 <- i2+1
          i2 <- i2 + group[[k]]$size
          #            grk <- group[[k]]$regressor[[jr]]
          grk <- group[[k]]$regressor
          tk <- as.data.frame(grk[c('time','value')])
          names(tk)[2] <- grk$name
          ntk <- nrow(tk)
          tk <- tk[rep(seq.int(1,ntk), group[[k]]$size), ]
          tk$id <- rep(seq(i1,i2),each=ntk)
          if (is.null(tr))
            tr <- tk
          else
            tr <- merge(tr,tk, all=TRUE, sort=FALSE)
        }
        tr[is.na(tr)]='.'
        #          lv$regressor[[jr]] <- tr
        lv$regressor <- tr
      }
      # }
      gr.ori <- NULL
      for (k in (1:ng))
        gr.ori <- c(gr.ori, rep(k, group[[k]]$size))
      lv$gr.ori <- as.factor(gr.ori)
    }
  }
  if (is.null(N)) N<-1
  if (is.null(npop)) npop<-1
  
  test.rep <- FALSE
  if (nrep>1){
    if (test.N == FALSE)
      test.rep <- TRUE
    else if (is.null(group))
      test.rep <- TRUE
  }
  
  R.complete <- list()
  rs <- NULL
  for (ipop in (1:npop)) {
    if (disp.iter==TRUE) {
      if (nrep>1) 
        cat("\n")
      if (npop>1)
        cat("population: ",ipop,"\n")
    }
    irw <- 0
    
    if (test.pop == TRUE)  lv$parameter[[k.pop]] <- pop.mat[ipop,]
    if (test.rep == TRUE) {
      if (test.N==FALSE)  
        lv$group <- group
      dataIn <- simulxunit(model=model,lv=lv,settings=c(settings, data.in=TRUE),riov=riov)
    }
    if (ipop==1) lv0 <- lv
    if (test.pop == TRUE)  lv0$parameter[[k.pop]] <- pop.mat[ipop,]
    for (irep in (1:nrep)) {
      irw <- irw + 1
      settings$seed <- settings$seed +12345
      
      if (disp.iter==TRUE && nrep>1)  
        cat("replicate: ",irw,"\n")
      if (test.rep == TRUE) {
        r <- simulxunit(data = dataIn,settings=c(settings,load.design=FALSE), out.trt=out.trt,riov=riov)
      } else {
        if (test.N==TRUE && !is.null(group)) 
          lv <- resample.data(data=lv0,idOri=id,N=sum(g.size),replacement=replacement)
        if (test.N==FALSE)  
          lv$group <- group
        
        r <- simulxunit(model=model,lv=lv,settings=settings, out.trt=out.trt,riov=riov)
      }
      
      if (length(loq.n) > 0) {
        for (k in (1:length(loq.n))) {
          rnk <- loq.n[k]
          if (!is.null(rnk)) {    
            r[[rnk]]$cens <- 0
            loqk <- loq.a[[k]]
            if (!is.null(loqk$limit))
              r[[rnk]]$limit <- loqk$limit
            if (!is.null(loqk$lloq)) {
              ik <- which(r[[rnk]][[rnk]] < loqk$lloq )
              r[[rnk]][[rnk]][ik] <- loqk$lloq
              r[[rnk]]$cens[ik] <- 1
            }
            if (!is.null(loqk$uloq)) {
              ik <- which(r[[rnk]][[rnk]] > loqk$uloq )
              r[[rnk]][[rnk]][ik] <- loqk$uloq
              r[[rnk]]$cens[ik] <- -1
            }
            r[[rnk]]$cens <- factor(r[[rnk]]$cens, levels=c("0","1","-1"))
          }
        }
      }
      if (!(is.null(project)) & (!is.null(settings$data.in) && !settings$data.in)) {
        r$covariate <- parameter[[2]][which(id%in%r$originalId$oriId),]
        r$covariate$id <- (1:length(r$covariate$id))
        attr(r$covariate,"type") <- "covariate"
      }
      
      if (!(is.null(project)) && write.simul==TRUE) {
        rs <- r[which(names(r) %in% c("originalId","group"))]
      } else  {
        rs <- r
        rs[stat.0] <- NULL
      }
      if (length(stat.n) > 0) {
        for (k in (1:length(stat.n))) {
          rnk <- stat.n[k]
          if (!is.null(rnk)) {
            resak <- stat.a[[rnk]]
            rs[[rnk]] <- do.call(stat.f, c(list(rs[[rnk]]),resak))
          }
        }
      }
      r.attr <- sapply(r,attr,"type")
      if (nrep>1) {
        for (k in (1:length(rs)))
          if (is.data.frame(rs[[k]])) {
            attr.name <- attr(rs[[k]],"name")
            attr.type <- attr(rs[[k]],"type")
            rs[[k]] <- cbind(list(rep=as.factor(irw)), rs[[k]])
            attr(rs[[k]],"name") <- attr.name
            attr(rs[[k]],"type") <- attr.type 
          }
      }
      
      
      if (write.simul==TRUE) {
        for (k in (1:length(r))) {
          r[[k]] <- data.frame(lapply(r[[k]], function(y) if(is.numeric(y)) signif(y, 5) else y)) 
          if (npop>1)  
            r[[k]] <- cbind(list(pop=as.factor(ipop)),r[[k]])
          if (nrep>1)  
            r[[k]] <- cbind(list(rep=as.factor(irw)), r[[k]])
          attr(r[[k]],"type") <- r.attr[[k]]
        }
        if (ipop==1 & irep==1)
          app <- FALSE
        else
          app <- TRUE
        if (settings$format.original) 
          r[['format.original']] <- format.original
        r[['simulx']] <- TRUE
         writeDatamlx(r,result.folder=result.folder,result.file=result.file,
                     sep=sep,digits=digits,app.dir=app,app.file=app, project=project)
        
      } 
      if (irep==1) {
        res <- rs
      } else {
        for(nk in (names(rs)))
          if (is.data.frame(rs[[nk]])) 
            res[[nk]] <- rbind(res[[nk]],rs[[nk]])
          else
            res[[nk]] <- rs[[nk]]
      }  
    } # irep
    
    if (length(res)>0) {
      for (k in (1:length(res))) {
        Rk <- res[[k]]
        if (is.data.frame(Rk)) {
          if (npop>1)
            Rk <- cbind(list(pop=as.factor(ipop)),Rk)
          if (ipop==1)
            R.complete[[k]] <- Rk
          else
            R.complete[[k]] <- rbind(R.complete[[k]],Rk)
        } else
          R.complete[[k]] <- Rk
      }
    }
    
  } # ipop
  
  if (length(res)>0) {
    names(R.complete) <- names(res)
    for (k in (1:length(res))) {
      attrk <- attr(r[[names(res)[k]]],'type')
      if (!is.null(attrk))
        attr(R.complete[[k]],"type") <- attrk
    } 
  }
  
  if (settings$format.original) {
    R.complete$result.file <- result.file
    R.complete$headerTypes <- format.original$infoProject$dataheader
  }
  
  pop <- NULL
  if (test.pop == TRUE) {
    pop <- as.data.frame(pop.mat)
    #pop <- format(pop, digits = 5, justify = "left")
    pop <- cbind(pop=as.factor(1:npop),pop)
  } else if (!(is.null(project))) {
    pop <- parameter[[1]]
  }
  if (!is.null(pop)) {
    if (write.simul==TRUE) {
      r <- list(population=pop)
      writeDatamlx(r,result.folder=result.folder,sep=sep,digits=digits,app.dir=TRUE)
    } 
    R.complete$population <- pop
  }
  
  # !! RETRO-COMPTATIBILITY - < 2019R1 =============================================== !!
  if (!useLixoftConnectors){
    Sys.setenv(LIXOFT_HOME = "")
    Sys.setenv('PATH' = myOldENVPATH);  
  }
  # !! =============================================================================== !!  
  # For categorical output, returns the categories defined in the model, instead of {0, 1, ...}
  if (!Rmodel)
    R.complete <- repCategories(R.complete, model)
  if (is.null(settings$data.in)) settings$data.in=FALSE
  if (!settings$data.in) {
    if (test.project | imodel.inline)
      file.remove(model)
    else if (!is.null(riov))
      file.remove(riov$model)
  }
  if(settings$data.in)
    remove.model <- remove.model0 <- FALSE
  
  if (remove.model && file.exists(model))
    file.remove(model)
  if (remove.model0 && file.exists(model0))
    file.remove(model0)
  
  
  return(R.complete)
}


#--------------------------------------------------
#--------------------------------------------------
#       Simulxunit
#--------------------------------------------------
#--------------------------------------------------

simulxunit <- function(model=NULL, lv=NULL, data=NULL, settings=NULL, out.trt=TRUE, riov=NULL) { 
  #--------------------------------------------------
  # Manage settings
  #--------------------------------------------------
  cc  <-  processing_setting(settings)
  s <- cc[[1]]
  data.in <- cc[[2]]
  id.out  <- cc[[3]]
  id.ori  <- NULL
  
  if(is.null(data)) {
    
    #--------------------------------------------------
    #    MODEL
    #--------------------------------------------------
    if (model=="pkmodel")
      model = generateModelFromPkModel(lv$parameter[[1]],lv$output[[1]])
    
    dataIn <- list()    
    
    iop.group <- 1
    if (is.null(lv$group))
      iop.group <- 0
    #--------------------------------------------------
    
    lv$model <- model
    id.ori <- lv$id
    gr.ori <- lv$gr.ori
    lv  <- hformat(lv)
    
    dataIn  <-  hgdata(lv)
    dataIn$model <- model
    dataIn$trt <- lv$treatment
    dataIn$riov <- riov
    if(data.in==TRUE) {
      dataIn$iop.group <- iop.group
      dataIn$id.ori <- id.ori
      s$loadDesign <- TRUE
    }    
  } else {
    s$loadDesign <- FALSE
    dataIn <- data
    iop.group <- data$iop.group
    dataIn$iop.group <- NULL
    id.ori <- data$id.ori
    dataIn$id.ori <- NULL
    riov <- data$riov
  }
  i.null <- which(unlist(lapply(dataIn$individual, function(x) {length(x$response$time[[1]])==0})))
  if (length(i.null>0)) {
    n <- length(dataIn$individual )
    dataIn$individual <- dataIn$individual[-i.null]
    if (length(dataIn$group) == n)
    dataIn$group <- dataIn$group[-i.null]
    if (nrow(dataIn$individual_parameters$value) == n)
      dataIn$individual_parameters$value <- dataIn$individual_parameters$value[-i.null,]
    if (length(dataIn$trt) == n)
      dataIn$trt <- dataIn$trt[-i.null]
  }
  if (out.trt==TRUE)
    trt <- dataIn$trt
  else
    trt <- NULL
  if (length(s)==0){
    argList <- list(DATA=dataIn) 
  } else {
    argList <- list(DATA=dataIn, SETTINGS=s)       
  }
  if (identical(file_ext(model),"R")) {Rfile <- TRUE} else {Rfile <- FALSE}
  if ( !is.null(model) && exists(model, mode="function") ){Rsource <- TRUE} else {Rsource <- FALSE}
  dataOut <- NULL
  if (Rfile || Rsource){
    
    dataOut <- simulR(argList)
    return(dataOut)
    
  } else {
    if (.useLixoftConnectors()) # >= 2019R1
      .hiddenCall('dataOut <- lixoftConnectors::computeSimulations(dataIn, s)')
    else # < 2019R1 ================================================================== !!
      .hiddenCall('dataOut <- .Call("mlxComputeR", argList, PACKAGE = "mlxComputeR")')
    # !! ============================================================================= !!
    if(data.in==TRUE)
      return(dataIn)
    if (!exists("gr.ori"))
      gr.ori <- NULL
    dataOut  <- convertmlx(dataOut,dataIn,trt,iop.group,id.out,id.ori,gr.ori,riov$cat)
    if (!is.null(id.ori)) {
      if (!is.data.frame(id.ori))
        id.ori <- data.frame(newId=(1:length(id.ori)), oriId=id.ori)
      dataOut$originalId <- id.ori
    }
    if (!is.null(riov)) 
      dataOut <- dataOutiov(dataOut,riov)
    return(dataOut)
    
  }
  
}

dataOutiov <- function(d,r) {
  v <- r$iov
  o <- r$occ.name
  v <- intersect(v,names(d))
  if (length(v)>0) {
    iov <- merge(d[[o]],d[[v[1]]], sort=FALSE)
    d[[v[1]]] <- NULL
    #d[[o]] <- NULL
    if (length(v)>1) {
      for (k in (2:length(v))) {
        iov <- merge(iov,d[[v[k]]])
        d[[v[k]]] <- NULL
      }
    }
    d$parameter.iiv=d$parameter
    d$parameter <- NULL
    iov <- iov[order(iov$id),]
    iov$id <- as.factor(iov$id)
    d$parameter.iov=iov
  }
  names(d)[which(names(d)==o)] <- "occasion"
  return(d)
}


sim.pop <- function(n,mu,infop,fim,kw.max) {
  p1.name <- names(fim$se)
  np <- length(p1.name)
  p1.trans=rep("N",np)
  p2.name <- sub("_pop","",p1.name)
  i.pop <- match(infop$name,p2.name)
  i1 <- which(!is.na(i.pop))
  p1.trans[i.pop[i1]] <- infop$trans[i1]
  i.omega <- grep("omega_",p1.name)
  p1.trans[i.omega] <- "L"
  i.omega2 <- grep("omega2_",p1.name)
  p1.trans[i.omega2] <- "L"
  param <- data.frame(pop.param=mu,sd=fim$se,trans=p1.trans)
  if (is.null(kw.max))
    x <- simpopmlx(n=n,parameter=param,corr=fim$mat)
  else
    x <- simpopmlx(n=n,parameter=param,corr=fim$mat,kw.max=kw.max)
  return(x)
}

