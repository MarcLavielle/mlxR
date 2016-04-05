model.pharmml <- "pharmML/2008ThamCCR_v03.xml"
model.mlxtran <- 'Mlxtran/tham.txt'


adm <- list(time=c(0,200,500,700,1000,1200,1500,1700,2000,2200,2500,2700), 
            amount = 40000, target="A1")

p   <- c(pop_SIZE0=5, omega_SIZE0=0.1, pop_TOVER=20, omega_TOVER=0.1, 
         pop_AE50=2000, omega_AE50=0.2, TEQ=10, a=0.02, b=0.01)

ind <- list(name=c('TEQ','SIZE0','TOVER','AE50'))
f   <- list(name=c('A1','A2'), time=seq(0,5000,by=1))
y   <- list(name=c('A2_obs'), time=seq(500,4000,by=250))
out <- list(ind, f)

g <- list( size      = 3, 
           level     = 'individual', 
           treatment = adm)

res <- simulx( model     = model.mlxtran,
               parameter = p,
               output    = list(ind, f, y),
               group     = g,
               settings  = list(seed=123456))


#--------------------------------------------------------
if (g$size==1){
  plot1=ggplotmlx() +  geom_line(data=res$A1, aes(x=time, y=A1), size=0.5) +
    xlab("time (h)") + ylab("Drug exposure (mg)") + theme(legend.position="none")  
  plot2=ggplotmlx() +  geom_line(data=res$A2, aes(x=time, y=A2), size=0.5) +
    geom_point(data=res$A2_obs, aes(x=time, y=A2_obs)) +
    xlab("time (h)") + ylab("Tumor size (cm)") + theme(legend.position="none")
}else{
plot1=ggplotmlx() +  geom_line(data=res$A1, aes(x=time, y=A1, group=id, colour=id)) +
  xlab("time (h)") + ylab("Drug exposure (mg)") + theme(legend.position="none")
plot2=ggplotmlx() +  geom_line(data=res$A2, aes(x=time, y=A2, group=id, colour=id)) +
  geom_point(data=res$A2_obs, aes(x=time, y=A2_obs, colour=id)) +
  xlab("time (h)") + ylab("Tumor size (cm)") + theme(legend.position="none")
}
grid.arrange(plot1, plot2, ncol=1)

print(res$parameter)