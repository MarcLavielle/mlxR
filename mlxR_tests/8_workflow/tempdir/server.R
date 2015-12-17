
server <- function(input, output) {
  res <- reactive({  
    p <- list(name=c('F', 'ka', 'V', 'k'),
              value=c(input$F, input$ka, input$V, input$k))
    f <- list(name=c('Cc'), time=seq(0,100,by=0.1))
    t1 <- input$tfd1
    t2 <- input$ii1*(input$nd1-1)+t1
    t.dose <- seq(t1,t2,by=input$ii1)
    adm1 <- list(time=t.dose, amount=input$amount1, type=as.numeric(input$type1))
    t1 <- input$tfd2
    t2 <- input$ii2*(input$nd2-1)+t1
    t.dose <- seq(t1,t2,by=input$ii2)
    adm2 <- list(time=t.dose, amount=input$amount2, rate=input$rate2, type=as.numeric(input$type2))
    adm <- list(adm1, adm2)
    res <- simulx( model     = 'temp_model.txt',
                   parameter = p,
                   treatment = adm,
                   output    = f)
    return(res)
  })  

  output$plot <- renderPlot({
    r <- res()$Cc
    pl <- ggplotmlx(r) + geom_line(aes(x=time,y=Cc),size=0.75,color='#CC0033')
    print(pl)
  })
}

