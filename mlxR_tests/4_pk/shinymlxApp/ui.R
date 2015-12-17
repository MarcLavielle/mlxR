
ui <- shinyUI(fluidPage(
fluidRow(
column(3,br(),
sliderInput("GAMMA", label="GAMMA",value=1,min=0.5,max=2,step=0.1),
sliderInput("K", label="K",value=100,min=50,max=200,step=10),
sliderInput("KDE", label="KDE",value=0.3,min=0.15,max=0.6,step=0.03),
sliderInput("LAMBDAP", label="LAMBDAP",value=0.4,min=0.2,max=0.8,step=0.04),
sliderInput("DELTAQP", label="DELTAQP",value=0.05,min=0.025,max=0.1,step=0.005),
sliderInput("KPQ", label="KPQ",value=0.025,min=0.0125,max=0.05,step=0.0025),
sliderInput("KQPP", label="KQPP",value=0.004,min=0.002,max=0.008,step=4e-04),
sliderInput("PT0", label="PT0",value=5,min=2.5,max=10,step=0.5),
sliderInput("Q0", label="Q0",value=40,min=20,max=80,step=4),

hr(),
sliderInput("amount1", label="amount",value=1,min=0.5,max=2,step=0.1),
br()
),
column(9,br(),            
plotOutput("plot")
)
)
))

