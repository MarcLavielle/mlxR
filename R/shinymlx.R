#' Automatic code generation for Shiny applications
#' 
#' Creates a Shiny application for longitudinal data model  
#' 
#' shinymlx automatically generates files ui.R and server.R required 
#' for a Shiny application.
#' 
#' 
#' See http://simulx.webpopix.org/mlxr/shinymlx/ for more details.      
#' @param model a \code{Mlxtran} or \code{PharmML} model used for the simulation
#' @param output a list - or a list of lists - with fields: 
#' \itemize{
#'   \item \code{name}: a vector of output names
#'   \item \code{time}: a vector of times, or a vector (min, max, step)
#' }
#' @param data a datafile to display with the plot
#' @param parameter a vector - or a list of vectors - of parameters with their names and values, or vectors (value, min, max, step)
#' @param treatment a list with fields 
#' \itemize{
#'   \item \code{tfd} : first time of dose (value, min, max, step),
#'   \item \code{amount} : amount (value, min, max, step),
#'   \item \code{nd} : number of doses (value, min, max, step),
#'   \item \code{ii} : interdose interval (value, min max, step),
#'   \item \code{type} : the type of input (value, max),
#' }
#' Input argument of Simulx can also be used,  i.e. a list with fields
#' \itemize{
#'   \item \code{time} : a vector of input times,
#'   \item \code{amount} : a scalar or a vector of amounts,
#'   \item \code{rate} : a scalar or a vector of infusion rates (default=\code{Inf}),
#'   \item \code{tinf} : a scalar or a vector of infusion times (default=0),
#'   \item \code{type} : the type of input (default=1),
#'   \item \code{target} : the target compartment (default=NULL). 
#' }
#' @return A directory temp_dir with files ui.R, server.R and temp_model.txt
#' 
#' @export         
shinymlx <- function(model,parameter=NULL,output=NULL,treatment=NULL,
                     data=NULL,appname="tempdir",title=" ",display=3, select.x=TRUE)
{
  library(shiny)
  s1 <- ""
  s2r <- ""
  s2f <- ""
  dir.create(file.path(mainDir=appname), showWarnings = FALSE)
  file.copy(model,file.path(appname,"model.txt"),overwrite=TRUE)
  
  output <- foutput(output)  
  out.txt <- out2str(output)
  s1 <- paste0(out.txt,"\n")
  if (select.x==TRUE)
  select.x <- testout(output)
  
  i.data=0
  if (!is.null(data)){
    i.data=1
    file.copy(data,file.path(appname,basename(data)),overwrite=TRUE)
    data.txt <- paste0("  datax <- read.csv(file='",basename(data),"', header=TRUE, sep='\\t', quote='\')\n")
    s1 <- paste0(s1, data.txt)
    data <- read.csv(file=data, header=TRUE, sep="\t", quote="\"")
  }
  parameter <- fparameter(parameter)
  ptxt <- param2str(parameter)
  s2r <- paste0(s2r, ptxt[[1]],"\n")
  s2f <- paste0(s2f, ptxt[[2]],"\n")
  if (!is.null(treatment)){
    treatment <- ftreatment(treatment)
    ttxt <- adm2str(treatment)
    s2r <- paste0(s2r, ttxt[[1]],"\n")
    s2f <- paste0(s2f, ttxt[[2]],"\n")
    s3 <- ("
    r <- simulx( model     = 'model.txt',
                 treatment = adm,
                 parameter = p,
                 output    = f)
")    
  }else{
    s3 <- ("
    r <- simulx( model     = 'model.txt',
                 parameter = p,
                 output    = f)
")    
    treatment <- NULL
  }
  
  
  if (i.data==1)
    s4 <- paste0("    pl <- pl + geom_point(data=datax,aes(x=",names(data)[1],",y=",names(data)[2],"),size=3.5,color='#6666CC')")
  else
    s4 <- ""
  
  server.txt <- server.template1(c(s1,s2r,s2f,s3,s4),select.x)
  write(server.txt,file.path(appname,"server.R"))
  sui <- list2ui(parameter,treatment,output,select.x,display)
  for (k in (1:length(sui)))
    cat(sui[[k]],"\n\n")
  ui.txt <- ui.template1(c(title,sui),display)
  write(ui.txt,file.path(appname,"ui.R"))
  
  stools.txt <- stoolsmlx(select.x)
  write(stools.txt,file.path(appname,"shinymlx_tools.R"))
  
  runApp(appname)
}

testout <- function(out){
  n <- length(out)
  select.x <- TRUE
  if (n>1){
    t <- out[[1]]$time
    for (k in (2:n)){
      if (!identical(out[[k]]$time, t))
        select.x <- FALSE
    }
  }else if (length(out[[1]]$name)==1)
    select.x <- FALSE
  
  return(select.x)
}

foutput <- function(out){
  if (!is.null(names(out)))
    out <- list(out)
  for (k in seq(1,length(out))){
    outk <- out[[k]]
    out.time <- outk$time
    if (length(out.time)>3){
      outk$time <- c(min(out.time), max(out.time), diff(range(out.time))/(length(out.time)-1))
    }else if (length(out.time)==2){
      outk$time <- c(min(out.time), max(out.time), 1)
    }
    if (is.null(names(outk)))
      names(outk <- c("min", "max", "by"))  
    out[[k]] <- outk
  } 
  return(out)
}


ftreatment <- function(trt){
  if (!is.null(names(trt)))
    trt <- list(trt)
  for (k in seq(1,length(trt))){
    trtk <- trt[[k]]
    nk <- names(trtk)
    for (j in (1: length(trtk))){
      pj <- trtk[[j]]
      if (!is.list(pj)){
        if (length(pj)==1){
          if (nk[j]=="type")
            trtk[[j]] <- list(value=pj, widget="numeric")
          else
            trtk[[j]] <- list(value=pj, min=pj/2, max=pj*2, step=pj*0.1, widget="slider")
        }else if (("ii" %in% nk) & (length(pj)==4)){
          trtk[[j]] <- list(value=pj[1], min=pj[2], max=pj[3], step=pj[4], widget="slider")        
        }else{
          trtk[[j]] <- list(value=pj, widget="none")        
        }
      }
    }
    trt[[k]] <- trtk
  } 
  return(trt)
}

fparameter <- function(param) 
{
  if (!is.null(names(param))){  
    param=list(param) 
  } 
  for (k in seq(1,length(param))){
    paramk <- param[[k]]
    if (is.list(paramk)){
      if (!is.null(paramk$value)){
        pk <- paramk$value
        names(pk) <- paramk$name
        paramk <- pk
      }
    }
    if (!is.list(paramk)){
      paramk <- as.list(paramk)
      for (j in (1:length(paramk)))
        paramk[[j]] <- c(1,0.5,2,0.1)*paramk[[j]]
    }
    for (j in (1: length(paramk))){
      pj <- paramk[[j]]
      if (!is.list(pj)){
        if (length(pj)==1){
          paramk[[j]] <- list(value=pj, widget="none")
        }else if (length(pj)==4){
          paramk[[j]] <- list(value=pj[1], min=pj[2], max=pj[3], step=pj[4], widget="slider")
        }else{
          stop("Error:  length of a parameter defined as a vector should be 4 (slider), or 1 (constant)")
        }        
      }
    }
    param[[k]] <- paramk
  } 
  return(param)
}


#-------------------------------------------------------

adm2str <- function(y, sp.adm="    "){
  K <- length(y)
  for (k in (1:K)){
    x <- y[[k]]
    length.x <- length(x)
    names.x  <- names(x)
    
    if ("time" %in% names.x){
      time.x <- vect2str(x[["time"]]$value)
      txt.tdose1 <- paste0(sp.adm,"t.dose <- ",time.x)
      txt.tdose2 <- txt.tdose1
    }else{
      if ("tfd" %in% names.x){
        if (x[["tfd"]]$widget == "none"){
          stfd <- x[["tfd"]]$value
        }else{
          stfd <- paste0("input$tfd",k)
        }
      }else{
        stfd <- "0"
      }
      txt.tdose1 <- paste0(sp.adm,"t1 <- ",stfd)
      txt.tdose2 <- paste0(sp.adm,"t1 <- isolate(",stfd,")")
      if ("nd" %in% names.x){
        sii <- widgtxt(x,"ii",k)
        snd <- widgtxt(x,"nd",k)
        txt.tdose1 <- paste0(txt.tdose1,"\n",sp.adm,"t2 <- ",sii,"*(",snd,"-1)+t1")
        txt.tdose1 <- paste0(txt.tdose1,"\n",sp.adm,"t.dose <- seq(t1,t2,by=",sii,")")
        sii <- paste0("isolate(",sii,")")
        snd <- paste0("isolate(",snd,")")
        txt.tdose2 <- paste0(txt.tdose2,"\n",sp.adm,"t2 <- ",sii,"*(",snd,"-1)+t1")
        txt.tdose2 <- paste0(txt.tdose2,"\n",sp.adm,"t.dose <- seq(t1,t2,by=",sii,")")
      }else{
        txt.tdose1 <- paste0(txt.tdose1,"\n",sp.adm,"t.dose <- t1")
        txt.tdose2 <- paste0(txt.tdose2,"\n",sp.adm,"t.dose <- t1")
      }
      
    }
    if (K==1){
      sadmk <- "adm"
    }else{    
      sadmk <- paste0("adm",as.character(k))
    }
    txt1 <- paste0(txt.tdose1,"\n",sp.adm,sadmk," <- list(time=t.dose, ")
    txt2 <- paste0(txt.tdose2,"\n",sp.adm,sadmk," <- list(time=t.dose, ")
    
    for (m in (1:length.x)){
      xk <- x[[m]]
      if (!(names(x[m]) %in% c("tfd","nd","ii","time"))){
        if (xk$widget=="none"){
          txtk1 <- vect2str(xk$value)
          txtk2 <- txtk1
        }else{
          txtk1 <- paste0("input$",names.x[m],k)
          txtk2 <- paste0("isolate(input$",names.x[m],k,")")
        }
        txt1 <- paste0(txt1,names.x[m],"=",txtk1,",")
        txt2 <- paste0(txt2,names.x[m],"=",txtk2,",")
      }
    }
    substr(txt1,nchar(txt1),nchar(txt1))<- ')'
    substr(txt2,nchar(txt2),nchar(txt2))<- ')'
    if (k==1){
      atxt1 <- txt1
      atxt2 <- txt2
    }else{
      atxt1 <- paste0(atxt1,"\n",txt1)
      atxt2 <- paste0(atxt2,"\n",txt2)
    }
  }
  if (K>1){
    txt <- "adm <- list(adm1"
    for (k in (2:K))
      txt <- paste0(txt,", adm",as.character(k))
    txt <- paste0(txt,")")
    atxt1 <- paste0(atxt1,"\n",sp.adm,txt)
    atxt2 <- paste0(atxt2,"\n",sp.adm,txt)
  }
  adm.txt <- list(atxt1, atxt2)
  return(adm.txt)
}

#-------------------------------
widgtxt <- function(x,txt,k){
  if (x[[txt]]$widget != "none"){
    if (x[[txt]]$widget != "select")
      s <- paste0("input$",txt,k)
    else
      s <- paste0("as.numeric(input$",txt,k,")")
  }else{
    s <- vect2str(x[[txt]]$value)
  }
  return(s)
}

#------------------------------------------------------
out2str <- function(out, sp.out="    "){
  K <- length(out)
  out.txt <- NULL
  for (k in (1:K)){
    outk <- out[[k]]
    if (K==1){
      soutk <- "f"
    }else{    
      soutk <- paste0("out",as.character(k))
    }
    txt <- paste0(sp.out,soutk," <- list(time=t.dose, ")
    txt.nameo <- vect2str(outk$name)
    t.out <- outk$time
    out.txt <- paste0(out.txt,"\n",sp.out,soutk," <- list(name=",txt.nameo,
                      ", time=seq(",t.out[1],",",t.out[2],",by=",t.out[3],"))")
  }
  if (K>1){
    txt <- "f  <- list(out1"
    for (k in (2:K))
      txt <- paste0(txt,", out",as.character(k))
    txt <- paste0(txt,")")
    out.txt <- paste0(out.txt,"\n",sp.out,txt)
  }else{
    out.txt <- paste0(out.txt,"\nf <- list(f)\n")
  }
  return(out.txt)
}

#-------------------------------------------------------
vect2str <- function(x){
  if (is.character(x[1])){
    txt <- paste0("'",x[1],"'")
  }else{
    txt <- as.character(x[1])
  }
  if (length(x)>1){
    for (j in (2:length(x)))
      if (is.character(x[j])){
        txt <- paste0(txt,",'",x[j],"'")
      }else{
        txt <- paste0(txt,",",as.character(x[j]))
      }
    txt <- paste0("c(",txt,")")
  }
  return(txt)
}


#-------------------------------------------------------

param2str <- function(p, sp.param="    "){
  txt.input <- NULL
  txt.namep <- NULL
  for (j in (1:length(p))){
    pj <- p[[j]]
    names.p <- names(pj)
    np <- length(names.p) 
    for (k in 1:np){
      pk <- pj[[k]]
      nk <- names.p[k]
      txt.namep <- paste0(txt.namep,"'",nk,"', ")
      if (pk$widget != "none"){
        if (pk$widget == "select")
          txt.input <- paste0(txt.input,'as.numeric(input$',nk,'), ')
        else
          txt.input <- paste0(txt.input,'input$',nk,', ')
      }else{
        txt.input <- paste0(txt.input,pk$value,", ")      
      }
    }
  }
  txt.input <- substr(txt.input,1,(nchar(txt.input)-2))
  txt.namep <- substr(txt.namep,1,(nchar(txt.namep)-2))
  txtr <- paste0(sp.param,"p <- list(name  = c(",txt.namep,"),\n",
                 sp.param,"          value = c(",txt.input,"))\n")
  txtf <- paste0(sp.param,"p <- list(name  = c(",txt.namep,"),\n",
                 sp.param,"          value = isolate(c(",txt.input,")))\n")
  txt <- list(txtr, txtf)
  return(txt) 
}


#--------------------------------------

ui.template1 <- function(s,display)
{
  if (display==1){
    ui.out <- paste0('
ui <- shinyUI(fluidPage(
navbarPage("',s[1],'",
tabPanel("Plot",
fluidRow(
column(3,\n',
                     s[2],'
),
column(9,br(),            
plotOutput("plot")
)
)
),
tabPanel("Mlxtran", pre(includeText("model.txt"))),
tabPanel("ui.R", pre(includeText("ui.R"))),
tabPanel("server.R", pre(includeText("server.R")))
)
))
')    
    
  }else if (display==2){
    ui.out <- paste0('
ui <- shinyUI(fluidPage(
navbarPage("',s[1],'",
tabPanel("Plot",
fluidRow(
column(2,\n',
                     s[2],'\n
),
column(2,
tabsetPanel(type="pills",
tabPanel("outputs",
br(),\n',
                     s[3],'\n)\n)\n
),
column(8,br(),            
plotOutput("plot")
)
)
),
tabPanel("Mlxtran", pre(includeText("model.txt"))),
tabPanel("ui.R", pre(includeText("ui.R"))),
tabPanel("server.R", pre(includeText("server.R")))
)
))
')
    
  }else{
    ui.out <- paste0('
library(shinydashboard)

sidebar <- dashboardSidebar(
hr(),
sidebarMenu(id="tabs",
menuItem("Plot", tabName = "plot", icon = icon("line-chart"), selected=TRUE),
menuItem("Codes",  icon = icon("file-text-o"),
menuSubItem("Mlxtran", tabName = "mlxtran", icon = icon("angle-right")),
menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
menuSubItem("shinymlx_tools.R", tabName = "tools", icon = icon("angle-right"))
)
),
hr(),
fluidRow(               
column(1),
column(9,\n',
                     s[3],'
)
),   
hr()
)

body <- dashboardBody(
tabItems(
tabItem(tabName = "plot",
fluidRow(               
box(width = 3,   status = "primary",\n',
                     s[2],'          
),
box(width = 9,   height=530, status = "primary",
plotOutput("plot")
)
)
),
tabItem(tabName = "mlxtran",
box( width = NULL, status = "primary", solidHeader = TRUE, title="model.txt",                
pre(includeText("model.txt"))
)
),
tabItem(tabName = "ui",
box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
pre(includeText("ui.R"))
)
),
tabItem(tabName = "server",
box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
pre(includeText("server.R"))
)
),
tabItem(tabName = "tools",
box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
pre(includeText("shinymlx_tools.R"))
)
)
#       tabItem(tabName = "about", includeMarkdown("../../about/about.Rmd"))
)
)

dashboardPage(
dashboardHeader(title = "',s[1],'"),
sidebar,
body
)

')
  }
  
  return(ui.out)
}

#--------------------------------------
server.template1 <- function(s, select.x)
{
  if (select.x==TRUE){
    spl <- paste0("      pj <- paste0('pl <- pl + geom_path(data=res, aes(x=',xj,',y=',name.fj[k],',colour=",'"',"',info[[j]]$colour[k],'",'"',"),size=0.75)')") 
    srf <- paste0("      pj <- paste0('pl <- pl + geom_path(data=ref, aes(x=',xj,',y=',name.fj[k],'),colour=",'"grey",',"size=0.75)')") 
  }else{
    spl <- paste0("      pj <- paste0('pl <- pl + geom_path(data=res[[j]], aes(x=',xj,',y=',name.fj[k],',colour=",'"',"',info[[j]]$colour[k],'",'"',"),size=0.75)')") 
    srf <- paste0("      pj <- paste0('pl <- pl + geom_path(data=ref[[j]], aes(x=',xj,',y=',name.fj[k],'),colour=",'"grey",',"size=0.75)')") 
  }
  
  server.out <- paste0('

library("mlxR")
library("reshape")
library("gridExtra")
source("shinymlx_tools.R")
',
                       s[1],'
nf <- length(f)
info <- info_res(f)

server <- function(input, output) {
    ref <- reactive({
    input$butref \n',
                       s[3],
                       s[4],
                       '    ref <- merge_res(r,f)
    return(ref)
  })  

    res <- reactive({\n',
                       s[2],
                       s[4],
                       '    res <- merge_res(r,f)
    return(res)
  })  

  output$plot <- renderPlot({
    res=res()
    ref=ref()

    gr.txt <- "grid.arrange("
    for (j in (1:length(f))){
      xj <- "time"
      fj <- f[[j]]
      name.fj <- fj$name
      eval(parse(text=paste0("inputyj=input$out",j)))
      i.plot=FALSE
      if (!is.null(inputyj)){
        ij <- which(name.fj %in% inputyj)
        if (length(ij>0)){
          eval(parse(text=paste0("inputxj=input$x",j)))
          if (!is.null(inputxj))
            xj <- inputxj
        }
        i.plot=TRUE
      }
      else if (is.null(inputyj) & length(f)==1){
        ij=1
        i.plot=TRUE
      }
      if (i.plot){
        pl <- ggplotmlx()
        nfj <- length(name.fj)
        for (k in (1:nfj)){
          if (k %in% ij){
              if (input$boxref==TRUE){\n',
                       srf,'
              eval(parse(text=pj))\n}\n',
                       spl,'
              eval(parse(text=pj))
          }
        }
        
        pl <- pl + scale_colour_manual(values=info[[j]]$values, labels=info[[j]]$labels)
        if (length(ij)>1){
          if (input$legend==TRUE)
            pl <- pl + guides(colour=guide_legend(title=NULL)) + theme(legend.position=c(.9, .8))
        else
          pl <- pl + theme(legend.position="none")
          pl <- pl + ylab("")
        }else{
          pl <- pl + theme(legend.position="none")
        }\n',
                       s[5],'
        if (input$ilog==TRUE)
           pl=pl + scale_y_log10()
        eval(parse(text=paste0("pl",j," <- pl")))
        gr.txt <- paste0(gr.txt,"pl",j,",")
      }
    }
    gr.txt <- paste0(gr.txt,"ncol=1)")
    eval(parse(text=gr.txt))
  }, height = 500)
}
')
  return(server.out)
}

#--------------------------------------
list2ui <- function(y.p=NULL,y.a=NULL, y.o=NULL, select.x=TRUE, display=1){
  x.p <- list()
  x.a <- list()
  
  if (!is.null(y.p)){
    for (k in (1:length(y.p))){
      yk <- y.p[[k]]
      ik <- 0
      for (j in (1:length(yk))){
        if (yk[[j]]$widget != "none")
          ik <- 1
      }
      if (ik == 1)
        x.p <- c(x.p, list(yk))   
    }
  }
  if (!is.null(y.a)){
    for (k in (1:length(y.a))){
      yk <- y.a[[k]]
      ik <- 0
      for (j in (1:length(yk))){
        if (yk[[j]]$widget != "none")
          ik <- 1
      }
      if (ik == 1)
        x.a <- c(x.a, list(yk))    
    }
  }
  
  name.out <- NULL
  i.leg <- FALSE
  for (k in (1:length(y.o))){
    name.out <- c(name.out, y.o[[k]]$name)
    if (length(y.o[[k]]$name)>1)
      i.leg <- TRUE
  }
  
  n.p <- length(x.p)
  n.a <- length(x.a)
  n.o <- length(name.out)
  K <- n.p + n.a
  i.panel <- TRUE
  if (display==3 && K==1)
    i.panel <- FALSE
  x.type <- c(rep("param",n.p),rep("adm",n.a))
  txt <- NULL
  if (i.panel==TRUE)
    txt <- paste0("tabsetPanel(type='pills',\n")
  
  k.a <- k.p <- 0
  for (k in (1:K)){
    if (x.type[k]=="param"){
      k.p <- k.p+1
      xk <- x.p[[k.p]]
      label <- "param"
      if (n.p>1)
        label <- paste0(label,k.p)
    }else{
      k.a <- k.a+1
      xk <- x.a[[k.a]]
      label <- "adm"
      if (n.a>1)
        label <- paste0(label,k.a)
    }
    if (i.panel==TRUE)
      txt <- paste0(txt,"tabPanel('",label,"',br(),\n")
    for (j in (1:length(xk))){
      xkj <- xk[[j]] 
      nxkj <- names(xk[j])
      if (x.type[k]=="param")
        id <- nxkj
      else
        id <- paste0(nxkj,k.a)
      if (xkj$widget!="none"){
        uikj <- widgetui(xkj,id, nxkj)
        txt <- paste0(txt,uikj,",\n")
      }
    }
    txt <- paste0(txt,"br()\n")
    if (K>1){
      if (k<K){
        txt <- paste0(txt,"),\n")
      }
    }
  }
  if (i.panel==TRUE){
    if (display==1){
      txt <- paste0(txt,"),\n")
    } else {
      txt <- paste0(txt,")\n)\n")
    }
  }
  
  
  if (display==1){
    out.txt <- paste0('tabPanel("out",br(),\n')
  } else{
    out.txt <- NULL
  }
  if (n.o>1){
    name.x <- c("time",name.out)
    names(name.x <- name.x)
    
    if (length(y.o)==1){
      nout <- vect2str(name.out)
      out.txt <- paste0(out.txt, 'checkboxGroupInput("out1", label="plot", choices=',
                        nout,', selected=',nout,'),')
      if (select.x==1)
        out.txt <- paste0(out.txt,'\nselectInput("x1", label="v.s.",',vect2str(name.x),'),')       
      out.txt <- paste0(out.txt, 'hr(),\n')
    }else{
      #       out.txt <- paste0('tabPanel("out",\n')
      for (k in (1:length(y.o))){
        noutk <- vect2str(y.o[[k]]$name)
        #         out.txt <- paste0(out.txt,'br(),\nh4("Graphic ',k,'"),\n')        
        #         out.txt <- paste0(out.txt,'hr(),\n')        
        out.txt <- paste0(out.txt,'checkboxGroupInput("out',k,'", label="plot ',k,'", choices=',
                          noutk,', selected=',noutk,'),\n')        
        if (select.x==1)
          out.txt <- paste0(out.txt,'\nselectInput("x',k,'", label="v.s.",',vect2str(name.x),'),\n')       
        out.txt <- paste0(out.txt, 'hr(),\n')
      }
    }
    out.txt <- paste0(out.txt, 'h5("add"),\n')
    if (i.leg)
      out.txt <- paste0(out.txt,'checkboxInput("legend", label="legend", value=TRUE),\n') 
  }
  
  sref1 <- ('
fluidRow(
column(5,checkboxInput("boxref", label="%s")),
column(4,actionButton("butref", label = "Reset"))
),
')  
  if (display==1)
    sref1 <- sprintf(sref1,"reference")
  else
    sref1 <- sprintf(sref1,"ref.")
  
  out.txt <- paste0(out.txt, sref1,'hr(),\n')
  out.txt <- paste0(out.txt,'radioButtons("ilog", "scale", c("linear" = FALSE,"log" = TRUE), inline=TRUE)')       
  
  if (display==1){
    txt <- paste0(txt,"\n",out.txt,"\n)\n)")
  }else{
    txt <- list(txt,out.txt)
  }
  return(txt)
}

#-------------------------------------------------------

widgetui <- function(xkj,id, nxkj){
  if (xkj$widget=="slider")
    uikj <-paste0('sliderInput("',id,'", label="',nxkj,'"')
  else if (xkj$widget=="numeric")                 
    uikj <-paste0('numericInput("',id,'", label="',nxkj,'"')
  else if (xkj$widget=="select")                 
    uikj <-paste0('selectInput("',id,'", label="',nxkj,'", choices=',vect2str(xkj$choices))
  
  for (k in (1:length(xkj))){
    if (names(xkj[k]) %in% c("value", "min","max","step","selected"))
      uikj <- paste0(uikj, ",", names(xkj[k]),"=", xkj[[k]] )
  }
  uikj <- paste0(uikj, ")")  
  return(uikj)
}

