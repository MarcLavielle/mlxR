library(grid)
library(gridExtra)
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#-------------------------------------

out1 <- list(name='f', time=seq(0, 100, by=1))
out2 <- list(name= c('y','z','u'), time=seq(0, 100, by=12))

res1 <- simulx(model     = "model/multiblock.txt",
               parameter = c(V=10, k=0.2, a=0.1), 
               output    = list(out1, out2))
names(res1)

head(merge(merge(res1$y,res1$z),res1$u))

#-------------------------------------

adm <- list(time=seq(0,66,by=12), amount=100)
out1 <- list(name= c("Cc","auc"), time=seq(0,100, by=0.5))
out2 <- list(name=c("y","z"), time=seq(18, 80, by=6))
p <- c(V_pop=10, omega_V=0.3, w=50, k=0.2, a=0.2)
add1 <- list(formula=c("ddt_auc = Cc", "z = log(y)"))

res2 <- simulx(model     = "model/newoutput.txt", 
               addlines  = add1,
               output    = list(out1, out2),
               parameter = p,
               treatment = adm)
names(res2)
head(merge(res2$Cc,res2$auc))
head(merge(res2$y,res2$z))


add2  = list(section="[INDIVIDUAL]", block="DEFINITION:", 
                 formula=c("Vn = {distribution=normal, prediction=Vpred, sd=1}"))

out3 <- list(name= c("Vn", "V"))
res3 <- simulx(model     = "model/newoutput.txt", 
               addlines  = list(add1, add2),
               output    = list(out1, out3),
               parameter = p,
               treatment = adm,
               group = list(size=5, level="individual"))
names(res3)
res3$parameter

#-------------------------------------

adm1 <- list(time=seq(0,66,by=6),  amount=50)
adm2 <- list(time=seq(0,66,by=12), amount=100)
p1 <- c(V_pop=10, omega_V=0.3, w=50, k=0.2, a=0.2)
p2 <- c(V_pop=20, omega_V=0.3, w=75, k=0.1, a=0.2)

outc  <- list(name = c('Cc','lc','auc'), time=seq(0,100, by=0.5))
y1 <- list(name="y", time=c(10,30,60))
y2 <- list(name="y", time=c(20, 50))
z1 <- list(name="z1", time=c(10,50))
z2 <- list(name="z2", time=c(20,40,60))
V <- list(name="V")

add3 <- list( formula = c("lc = log(Cc)", "ddt_auc=Cc", "z1=2*y", "z2=20") )

g1 <- list(treatment=adm1, parameter=p1, output=list(y1,z1), size=3, level='individual')
g2 <- list(treatment=adm2, parameter=p2, output=list(y2,z2), size=2, level='individual')

res4 <- simulx(model    = "model/newoutput.txt", 
               addlines  = add3,
               output   = list(outc,V),
               group    = list(g1,g2))

names(res4)
res4$z1
res4$z2

#-------------------------------------


res5  <- simulx(project  = 'monolixRuns/theophylline_project.mlxtran', 
                output   = list(name = c('Cc','auc'), time = seq(0, 30, by=0.1)), 
                addlines = list(formula="ddt_auc = Cc"))

names(res5)
head(merge(res5$Cc,res5$auc))

pl1 <- ggplotmlx() +  geom_line(data=res5$Cc, aes(x=time, y=Cc, colour=id)) +
        xlab("time") + ylab("concentration")

pl2 <- ggplotmlx() +  geom_line(data=res5$auc, aes(x=time, y=auc, colour=id)) +
        xlab("time") + ylab("area under the curve") 

grid_arrange_shared_legend(pl1, pl2, nrow=2, ncol=1, position="right")




