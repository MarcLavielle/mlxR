setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)
library(gridExtra)

#-------------------------------------

adm1 <- list(time=seq(0,to=60,by=6), amount=50)
adm2 <- list(time=seq(0,to=60,by=12), amount=100)
y1 <- list(name="y", time=seq(30, 100, by=2))
y2 <- list(name="y", time=seq(0, 70, by=2))
p1 <- list(name=c("V_pop","omega_V","w","k","a"), 
           value=c(10,0.3,50,0.2,0.5))
p2 <- list(name=c("V_pop","omega_V","w","k","a"), 
           value=c(15,0.3,75,0.1,0.5))
g1 <- list(treatment=adm1,output=y1,parameter=p1,size=3, level='individual')
g2 <- list(treatment=adm2,output=y2,parameter=p2,size=2, level='individual')
g  <- list(g1,g2)

res <- simulx(model="group2_model.txt", group=g)


plot1 <- ggplotmlx(data=res$y, aes(x=time, y=y,colour=id)) + 
         geom_point() + geom_line()
print(plot1+ facet_grid(. ~ group))
