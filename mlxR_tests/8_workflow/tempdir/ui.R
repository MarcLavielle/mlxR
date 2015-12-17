  
ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel('Plot',
             fluidRow(
               column(3,br(),

  tabsetPanel(
   tabPanel('adm1',br(),
                      selectInput('type1',label='type',selected=1,choices=c(1,2)),
                      sliderInput('tfd1',label='tfd',value=6,min=0,max=24,step=2),
                      sliderInput('nd1',label='nd',value=3,min=1,max=10,step=1),
                      sliderInput('ii1',label='ii',value=12,min=3,max=24,step=1),
                      sliderInput('amount1',label='amount',value=40,min=0,max=50,step=5),
                      br()),

   tabPanel('adm2',br(),
                      selectInput('type2',label='type',selected=2,choices=c(1,2)),
                      sliderInput('tfd2',label='tfd',value=12,min=0,max=24,step=2),
                      sliderInput('nd2',label='nd',value=2,min=1,max=10,step=1),
                      sliderInput('ii2',label='ii',value=30,min=5,max=50,step=1),
                      sliderInput('amount2',label='amount',value=20,min=0,max=50,step=5),
                      sliderInput('rate2',label='rate',value=5,min=1,max=10,step=1),
                      br()),

  tabPanel('param',br(),
                      sliderInput('F',label='F',value=0.7,min=0.35,max=1.4,step=0.07),
                      sliderInput('ka',label='ka',value=1,min=0.5,max=2,step=0.1),
                      sliderInput('V',label='V',value=10,min=5,max=20,step=1),
                      sliderInput('k',label='k',value=0.1,min=0.05,max=0.2,step=0.01),
                      br()))  
               ),
               column(9,br(),br(),br(),
                      plotOutput('plot',  height='500px'))
             )),
    tabPanel('Mlxtran', pre(includeText('temp_model.txt'))),
    tabPanel('ui.R', pre(includeText('ui.R'))),
    tabPanel('server.R', pre(includeText('server.R')))
  )
))

