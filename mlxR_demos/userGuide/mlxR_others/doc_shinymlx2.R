# Set working directory to source file location
library(mlxR)
library(reshape2)

#---------------------------------------------------
####  EXAMPLE 2
#---------------------------------------------------

myModel <- inlineModel("
[LONGITUDINAL]
input = {ka, V, Cl}
EQUATION:
C = pkmodel(ka, V, Cl)
")

f <- list(name = 'C', 
          time = c(0, 25, 0.5))

p <- list( ka = c(0.5,0.25,3,0.25),
           V  = c(5,1,20,1),
           Cl = c(1,0.5,3,.1))

shiny.app <- shinymlx(model     = myModel,
                      data      = 'data/pkdata1.txt',
                      treatment = list(time=c(0,12), amount=c(10,3)),
                      parameter = p, 
                      output    = f)
shiny::runApp(shiny.app)
