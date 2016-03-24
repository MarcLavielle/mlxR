
library(shinydashboard)

sidebar <- dashboardSidebar(
hr(),
sidebarMenu(id="tabs",
menuItem("Plot", tabName = "plot", icon = icon("line-chart"), selected=TRUE),
menuItem("Codes",  icon = icon("file-text-o"),
menuSubItem("Mlxtran", tabName = "mlxtran", icon = icon("angle-right")),
menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
menuSubItem("shinymlxTools.R", tabName = "tools", icon = icon("angle-right"))
)
),

hr(),
fluidRow(               
column(1),
column(9,
checkboxGroupInput("out1", label="plot 1", choices='Cc', selected='Cc'),

selectInput("x1", label="v.s.",c('time','Cc','E1','E2','E3')),
hr(),
checkboxGroupInput("out2", label="plot 2", choices=c('E1','E2','E3'), selected=c('E1','E2','E3')),

selectInput("x2", label="v.s.",c('time','Cc','E1','E2','E3')),
hr(),
strong("add"),
checkboxInput("legend", label="legend", value=TRUE),

fluidRow(
column(5,checkboxInput("boxref", label="ref.")),
column(4,actionButton("butref", label = "Reset"))
),
hr(),
radioButtons("ilog", "scale", c("linear" = FALSE,"log" = TRUE), inline=TRUE),
br()
)
),   

hr()
)

body <- dashboardBody(
tabItems(
tabItem(tabName = "plot",
fluidRow(               
box(width = 3,   status = "primary",
tabsetPanel(type='tabs',
tabPanel('param1',br(),
sliderInput("ka", label="ka",value=0.5,min=0.25,max=1,step=0.05),
sliderInput("V", label="V",value=10,min=5,max=20,step=1),
sliderInput("Cl", label="Cl",value=1,min=0.5,max=2,step=0.1),
br()
),
tabPanel('param2',br(),
sliderInput("ke0", label="ke0",value=0.1,min=0.05,max=0.2,step=0.01),
sliderInput("Imax", label="Imax",value=0.5,min=0.25,max=1,step=0.05),
sliderInput("IC50", label="IC50",value=0.03,min=0.015,max=0.06,step=0.003),
sliderInput("S0", label="S0",value=100,min=50,max=200,step=10),
sliderInput("kout", label="kout",value=0.1,min=0.05,max=0.2,step=0.01),
br()
),
tabPanel('adm',br(),
sliderInput("tfd1", label="tfd",value=5,min=2.5,max=10,step=0.5),
sliderInput("nd1", label="nd",value=15,min=7.5,max=30,step=1.5),
sliderInput("ii1", label="ii",value=12,min=6,max=24,step=1.2),
sliderInput("amount1", label="amount",value=1,min=0.5,max=2,step=0.1),
br()
),
br()
)
          
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
pre(includeText("shinymlxTools.R"))
)
)
#       tabItem(tabName = "about", includeMarkdown("../../about/about.Rmd"))
)
)

dashboardPage(
dashboardHeader(title = "PKPD models"),
sidebar,
body
)


