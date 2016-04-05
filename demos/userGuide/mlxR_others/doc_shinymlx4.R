library(mlxR)
library(reshape2)

#---------------------------------------------------
####  EXAMPLE 4
#---------------------------------------------------

PKPDmodel <- inlineModel("
[LONGITUDINAL] 
input={ka,V,Cl,ke0,Imax,IC50,S0,kout}

EQUATION:
{Cc, Ce}  = pkmodel(ka, V, Cl, ke0)

Ec = Imax*Cc/(Cc+IC50)
E1 = S0*(1 - Ec)

Ee = Imax*Ce/(Ce+IC50)
E2 = S0*(1 - Ee)

E3_0 = S0 
ddt_E3 = kout*((1-Ec)*S0- E3)  
")

pk.param <- c(ka=0.5, V=10, Cl=1)
pd.param <- c(ke0=0.1, Imax=0.5, IC50=0.03, S0=100, kout=0.1)

adm <- list(tfd=5, nd=15, ii=12, amount=1)

pk <- list(name = 'Cc',                time = seq(0, 250, by=1))
pd <- list(name = c('E1','E2', 'E3'),  time = seq(0, 250, by=1))


shiny.app <- shinymlx(model     = PKPDmodel, 
                      treatment = adm,
                      parameter = list(pk.param, pd.param), 
                      output    = list(pk, pd),
                      title     = "PKPD models",
                      style     = "dashboard1",
                      appname   = "demo4")
shiny::runApp(shiny.app)

