library("mlxR")
library("reshape")
library("gridExtra")
source("shinymlxTools.R")

f <- list(name=c('f1','f2'), time=seq(0,20,by=0.1))
f <- list(f)
nf <- length(f)
info <- info_res(f)

server <- function(input, output) {
  
  res <- reactive({
    p <- list(name  = c('ka', 'Tk0', 'V', 'k'),
              value = c(input$ka, input$Tk0, input$V, input$k))
                     
    r <- simulx( model     = 'model.txt',
                 parameter = p,
                 output    = f)
                     
    res <- merge_res(r,f)
    return(res)
  })  
  
  output$plot <- renderPlot({
    res=res()
    
    gr.txt <- "grid.arrange("
    for (j in (1:length(f))){
      xj <- "time"
      fj <- f[[j]]
      name.fj <- fj$name

      pl <- ggplotmlx()
      nfj <- length(name.fj)
      for (k in (1:nfj)){
       pj <- paste0('pl <- pl + geom_path(data=res[[j]], aes(x=time,y=',name.fj[k],',colour="',info[[j]]$colour[k],'"),size=0.75)')
       eval(parse(text=pj))
      }
      pl <- pl + scale_colour_manual(values=info[[j]]$values, labels=info[[j]]$labels)
      print(pl)
      if (length(name.fj)>1)
        pl <- pl + guides(colour=guide_legend(title=NULL)) + theme(legend.position=c(.9, .8))
      else
        pl <- pl + theme(legend.position="none")
      pl <- pl + ylab("")
      
      
      eval(parse(text=paste0("pl",j," <- pl")))
      gr.txt <- paste0(gr.txt,"pl",j,",")
    }
    gr.txt <- paste0(gr.txt,"ncol=1)")
    eval(parse(text=gr.txt))
  }, height = 500)
}

