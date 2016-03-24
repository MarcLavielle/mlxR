
ui <- shinyUI(fluidPage(
fluidRow(
column(3,br(),
sliderInput("ka", label="ka",value=0.5,min=0.25,max=1,step=0.05),
sliderInput("Tk0", label="Tk0",value=2,min=1,max=4,step=0.2),
sliderInput("V", label="V",value=10,min=5,max=20,step=1),
sliderInput("k", label="k",value=0.2,min=0.1,max=0.4,step=0.02),
br()
),
column(9,br(),            
plotOutput("plot")
)
)
))

