# library(mlxR)
library(reshape)

#---------------------------------------------------
####  EXAMPLE 1
#---------------------------------------------------

model1 <- inlineModel("
[LONGITUDINAL]
input = {ka, Tk0, V, k}

EQUATION:
  D  = 100

if t>Tk0
f0=D/(V*Tk0*k)*(1-exp(-k*Tk0))*exp(-k*(t-Tk0))
else
  f0=D/(V*Tk0*k)*(1-exp(-k*t))
end

f1 = f0
f2 = D*ka/(V*(ka-k))*(exp(-k*t) - exp(-ka*t))
")

f <- list(name = c('f1','f2'), time = seq(0, 20, 0.1))

p <- c(ka=0.5, Tk0=2, V=10, k=0.2)

r <- simulx(model = model1, parameter = p, output = f)

r <- melt(merge(r$f1,r$f2),  id='time', variable_name='f')
ggplotmlx(r, aes(time,value)) + geom_line(aes(colour = f),size=1) +
  guides(colour=guide_legend(title=NULL)) +theme(legend.position=c(.9, .75))

shiny.app <- shinymlx(model = model1, parameter = p, output = f)
shiny::runApp(shiny.app)
#---------------------------------------------------

p <- list( ka  = list(widget='slider',  value=0.5,min=0.1,max=1,step=0.1),
           Tk0 = list(widget='numeric', value=2),
           V   = list(widget='none',    value=2),
           k   = list(widget='select',  selected=0.2, choices=c(0.05,0.2,0.4))
)
shiny.app <- shinymlx(model=model1, parameter=p, output=f, style="dashboard1", 
                      title="Compare PK models")
shiny::runApp(shiny.app)

#---------------------------------------------------
s <- list(select.x=FALSE, select.log=FALSE)
shiny.app <- shinymlx(model=model1, parameter=p, output=f, style="navbar1", settings=s)
shiny::runApp(shiny.app)

#---------------------------------------------------
shiny.app <- shinymlx(model=model1, parameter=p, output=f, style="navbar2", settings=s)
shiny::runApp(shiny.app)

