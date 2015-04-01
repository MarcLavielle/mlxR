#' shinymlx
#' 
#' 
#' 
#' 
#' 
#' @export         
shinymlx <- function(model,parameter=NULL,output=NULL,treatment=NULL,data=NULL)
{
  dir.create(file.path(mainDir="tempdir"), showWarnings = FALSE)
  file.copy(model,file.path("tempdir","temp_model.txt"),overwrite=TRUE)
#   model <- basename(model)
  
  i.data=0
  if (!is.null(data)){
    i.data=1
    file.copy(data,file.path("tempdir",basename(data)),overwrite=TRUE)
    txt.data <- paste0("  data <- read.csv(file='",basename(data),"', header=TRUE, sep='\\t', quote='\')")
    data <- read.csv(file=data, header=TRUE, sep="\t", quote="\"")
  }
  
  i.trt <- 0
  if (!is.null(treatment)){
    if (!is.list(treatment[[1]]))
      treatment <- list(treatment)
    if (!('time' %in% names(treatment[[1]]))){
      i.trt=1
      txt.adm <- trtstr(treatment)
    }else{
      i.trt=2
      txt.adm <- list2str(treatment)
    }
  }
  n.trt <- length(treatment)
  
  out.time <- output$time
  if ((length(out.time)!=3) | (out.time[3]>out.time[2])){
    output$time <- c(range(out.time), diff(range(out.time))/(length(out.time)-1))
  }
  
  if (!is.list(parameter)){
    parameter <- as.list(parameter)
    for (k in (1:length(parameter)))
      parameter[[k]] <- c(1,0.5,2,0.1)*parameter[[k]]
  }
  if (n.trt>0){
    col1 <- 3
  }else{
    col1 <- 2
  }
  col2 <- 12-col1
  
  txt.ui1 <- paste0("  
ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel('Plot',
             fluidRow(
               column(",col1,",br(),\n")
  
  space.ui <- ("                      ")
  txt.ui3 <- paste0("  
               ),
               column(",col2,",br(),br(),br(),
                      plotOutput('plot',  height='500px'))
             )),
    tabPanel('Mlxtran', pre(includeText('temp_model.txt'))),
    tabPanel('ui.R', pre(includeText('ui.R'))),
    tabPanel('server.R', pre(includeText('server.R')))
  )
))
")
  
  names.param <- names(parameter)
  np <- length(names.param) 
  txt.ui2 <- NULL
  txt.input <- NULL
  txt.namep <- NULL
  for (k in 1:np){
    pk <- parameter[[k]]
    nk <- names.param[k]
    txt.namep <- paste0(txt.namep,"'",nk,"', ")
    if (length(pk)>1){
      txtk <- paste0("sliderInput('",nk,"',label='",nk,"',value=",pk[1],",min=",pk[2],",max=",pk[3],",step=",pk[4],"),")
      txt.ui2 <- paste0(txt.ui2,space.ui,txtk,'\n')
      txt.input <- paste0(txt.input,'input$',nk,', ')
    }else{
      txt.input <- paste0(txt.input,pk,", ")      
    }
  }
  txt.input <- substr(txt.input,1,(nchar(txt.input)-2))
  txt.namep <- substr(txt.namep,1,(nchar(txt.namep)-2))
  txt.ui2 <- paste0(txt.ui2,space.ui,'br()')
  
  if (i.trt==1){
    txt.trt <- paste0("\n  tabsetPanel(")
    for (k in (1:length(treatment))){
      trtk <- treatment[[k]]
      names.trt <- names(trtk)
      np <- length(names.trt) 
      txt.trt2 <- NULL
      txt.namet <- NULL
      for (j in 1:np){
        pj <- trtk[[j]]
        nj <- names.trt[j]
        if (nj=="nd")
          pj[2] <- max(1, pj[2])
        txt.namet <- paste0(txt.namet,"'",nj,k,"', ")
        if (length(pj)==4){
          txtj <- paste0("sliderInput('",nj,k,"',label='",nj,"',value=",pj[1],",min=",pj[2],",max=",pj[3],",step=",pj[4],"),")
        }else if (length(pj)==2){
          ch.txt <- paste0("c(",1)
          for (m in (2:pj[2]))
            ch.txt <- paste0(ch.txt,",",m)
          ch.txt <- paste0(ch.txt,")")
          txtj <- paste0("selectInput('",nj,k,"',label='",nj,"',selected=",pj[1],",choices=",ch.txt,"),")
        }else if (length(pj)==1){
          txtj <- paste0("selectInput('",nj,k,"',label='",nj,"',selected=",pj[1],",choices=",pj[1],"),")
        }
        txt.trt2 <- paste0(txt.trt2,space.ui,txtj,'\n')
        
      }
      txt.namet <- substr(txt.namet,1,(nchar(txt.namet)-2))
      txt.trt2 <- paste0(txt.trt2,space.ui,'br()')
      if (length(treatment)==1){
        txt.trt <- paste0(txt.trt,"\n   tabPanel('admin',br(),\n",txt.trt2,"),\n")
      }else{
        txt.trt <- paste0(txt.trt,"\n   tabPanel('adm",k,"',br(),\n",txt.trt2,"),\n")
      }
    }
    txt.ui <- paste0(txt.ui1,txt.trt,'\n',"  tabPanel('param',br(),\n",txt.ui2,"))",txt.ui3)
  }else{
    txt.ui <- paste0(txt.ui1,txt.ui2,txt.ui3)
  }
  
  names.out <- output$name
  no <- length(names.out) 
  txt.nameo <- NULL
  for (k in 1:no){
    nk <- names.out[k]
    txt.nameo <- paste0(txt.nameo,"'",nk,"', ")
  }
  
  txt.nameo <- substr(txt.nameo,1,(nchar(txt.nameo)-2))
  t.out <- output$time
  if (i.data==1){
    txt.server1 <- paste0("server <- function(input, output) {\n",txt.data,"\n  res <- reactive({\n")
  }else{
    txt.server1 <- ("
server <- function(input, output) {
  res <- reactive({  
")
  }
  txt.server2 <- paste0("    p <- list(name=c(",txt.namep,"),\n              value=c(",txt.input,"))")
  txt.server3 <- paste0("    f <- list(name=c(",txt.nameo,"), time=seq(",t.out[1],",",t.out[2],",by=",t.out[3],"))")
  
  txt.server4 <- paste0("
    res <- simulx( model     = 'temp_model.txt',
                   parameter = p,
")
  if (i.trt>=1){
    txt.server3 <- paste0(txt.server3,txt.adm)
    txt.server4 <- paste0(txt.server4,"                   treatment = adm,")
    #   }else if(i.trt==2){
    #     txt.server3 <- paste0(txt.server3,txt.adm)
    #     txt.server4 <- paste0(txt.server4,"                   treatment = adm,")
  }
  txt.server4 <- paste0(txt.server4,("
                   output    = f)
    return(res)
  })  

  output$plot <- renderPlot({"))
  nf <- length(output$name)
  if (nf==1){
    txt.server5 <- paste0("
    r <- res()$",output$name[1],"
    pl <- ggplotmlx(r) + geom_line(aes(x=time,y=",output$name[1],"),size=0.75,color='#CC0033')")
  }else{
    txt.server5 <- ("
    res=res()
    nf <- length(f$name)
    r <- res[[f$name[1]]]
    for (k in (2:nf))
      r <- merge(r,res[[f$name[k]]])
    r <- melt(r ,  id = 'time', variable_name = 'f')
    pl <- ggplotmlx(r, aes(time,value)) + geom_line(aes(colour = f),size=0.75) 
    pl <- pl + guides(colour=guide_legend(title=NULL)) + theme(legend.position=c(.9, .8))")
  }
  if (i.data==1){
    txt.data <- paste0("    pl <- pl + geom_point(data=data,aes(x=",names(data)[1],",y=",names(data)[2],"),color='#6666CC')")
    txt.server5 <- paste0(txt.server5,"\n",txt.data)
  }
  txt.server6 <- ("
    print(pl)
  })
}
")
  txt.server <- paste0(txt.server1,txt.server2,'\n',txt.server3,txt.server4,txt.server5,txt.server6)
  # txt.server <- paste0(txt.server1,'\n',txt.server2,'\n',txt.server3,'\n',txt.server4,'\n',txt.server5)
  
  #   txt.app <- paste0(txt.ui,'\n\n',txt.server,'\n\n','shinyApp(ui = ui, server = server)')
  
  write(txt.ui,'tempdir/ui.R')
  write(txt.server,'tempdir/server.R')
  # write(txt.app,'tempdir/app.R')
  runApp("tempdir")
  
}

list2str <- function(y){
  K <- length(y)
  adm.txt <- NULL
  for (k in (1:K)){
    if (K==1){
      txt <- "adm <- list("
    }else{    
      txt <- paste0("adm",as.character(k)," <- list(")
    }
    x <- y[[k]]
    length.x <- length(x)
    names.x  <- names(x)
    for (k in (1:length.x)){
      xk <- x[[k]]
      n <- length(xk)
      txtk <- as.character(xk[1])
      if (length(xk)>1){
        for (j in (2:length(xk)))
          txtk <- paste0(txtk,",",as.character(xk[j]))
        txt <- paste0(txt,names.x[k],"=c(",txtk,"),")
      }else{
        txt <- paste0(txt,names.x[k],"=",txtk,",")
      }  
    }
    substr(txt,nchar(txt),nchar(txt))<- ')'
    adm.txt <- paste0(adm.txt,"\n    ",txt)
  }
  if (K>1){
    txt <- "adm <- list(adm1"
    for (k in (2:K))
      txt <- paste0(txt,", adm",as.character(k))
    txt <- paste0(txt,")")
    adm.txt <- paste0(adm.txt,"\n    ",txt)
  }
  return(adm.txt)
}

trtstr <- function(y){
  K <- length(y)
  adm.txt <- NULL
  for (k in (1:K)){
      txt <- "    adm"
    if (K>1)    
      txt <- paste0(txt,as.character(k))
    x <- y[[k]]
    if ("tfd" %in% names(x)){
      txttfd <- paste0("    t1 <- input$tfd",k)
    }else{
      txttfd <- "    t1 <- 0"  
    }
    if ("nd" %in% names(x)){
      txtnd <- paste0("    t2 <- input$ii",k,"*(input$nd",k,"-1)+t1\n    t.dose <- seq(t1,t2,by=input$ii",k,")")
    }else{
      txtnd <- "      t.dose <- t1" 
    }
    txtadm <- paste0(txt," <- list(time=t.dose, amount=input$amount",k)
    if ("rate" %in% names(x))
      txtadm <- paste0(txtadm,", rate=input$rate",k)
    if ("tinf" %in% names(x))
      txtadm <- paste0(txtadm,", tinf=input$tinf",k)
    if ("type" %in% names(x))
      txtadm <- paste0(txtadm,", type=as.numeric(input$type",k,")")
    txtadm <- paste0(txtadm,")")
    adm.txt <- paste0(adm.txt,"\n",txttfd,"\n",txtnd,"\n",txtadm)   
  }
  if (K>1){
    txt <- "adm <- list(adm1"
    for (k in (2:K))
      txt <- paste0(txt,", adm",as.character(k))
    txt <- paste0(txt,")")
    adm.txt <- paste0(adm.txt,"\n    ",txt)
  }
  return(adm.txt)
}


