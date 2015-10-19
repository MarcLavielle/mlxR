library(mlxR)
library(reshape)

#---------------------------------------------------
####  EXAMPLE 3
#---------------------------------------------------

myModel <- inlineModel("
[LONGITUDINAL]
input = {F, ka, V, k}

PK:
depot(type=1, target=Ad, p=F)
depot(type=2, target=Ac)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac =  ka*Ad - k*Ac
Cc = Ac/V
")

p    <- c(F=0.7, ka=1, V=10, k=0.1)
Cc   <- list(name="Cc", time=seq(0, 100, by=0.1))

adm1 <- list(
  type   = list(widget="select", selected=1, choices=c(1,2)),
  tfd    = list(widget="slider", value=6,  min=0, max=24, step=2),
  nd     = list(widget="slider", value=3,  min=0, max=10, step=1),
  ii     = list(widget="slider", value=12, min=3, max=24, step=1),
  amount = list(widget="slider", value=40, min=0, max=50, step=5)
)
adm2 <- list(
  type   = list(widget="select", selected=2, choices=c(1,2)),
  tfd    = list(widget="slider", value=12, min=0, max=24, step=2),
  nd     = list(widget="slider", value=2,  min=0, max=10, step=1),
  ii     = list(widget="slider", value=30, min=6, max=60, step=6),
  amount = list(widget="slider", value=20, min=0, max=50, step=5),
  rate   = list(widget="slider", value=5,  min=1, max=10, step=1)
)

shiny.app <- shinymlx(model   = myModel,
                      parameter = p,
                      output    = Cc,
                      treatment = list(adm1,adm2),
                      style     = "navbar1")
shiny::runApp(shiny.app)



