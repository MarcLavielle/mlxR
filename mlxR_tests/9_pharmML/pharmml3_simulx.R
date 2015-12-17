
adm1 <- list(time=seq(0,36,by=12),amount=1  )
adm2 <- list(time=seq(0,45,by= 3),amount=0.25)

f <- list(name=c('PT','Q','QP','PSTAR'), time = seq(-50,to=100,by=0.5))
p <- c(K=100, pop_KDE=0.3, omega_KDE=0, pop_KPQ=0.025, omega_KPQ=0, pop_KQPP=0.004, 
       omega_KQPP=0, pop_LAMBDAP=0.12, omega_LAMBDAP=0, pop_GAMMA=1, omega_GAMMA=0, 
       pop_DELTAQP=0.01, omega_DELTAQP=0, a=3 )

g1 <- list(treatment=adm1)
g2 <- list(treatment=adm2)
g <- list(g1, g2)

res <- simulx(model='model/tgi2_model.xml',parameter=p,output=f,group=g);

plot1=ggplotmlx(data=res$PT, aes(x=time, y=PT, colour=id)) +  geom_line(size=1) +
  xlab("time (month)")  + theme(legend.position="none")

plot2=ggplotmlx(data=res$Q, aes(x=time, y=Q, colour=id)) +  geom_line(size=1) +
  xlab("time (month)") 

plot3=ggplotmlx(data=res$QP, aes(x=time, y=QP, colour=id)) + geom_line(size=1) +
  xlab("time (month)") + theme(legend.position="none")

plot4=ggplotmlx(data=res$PSTAR, aes(x=time, y=PSTAR, colour=id)) + 
  geom_line(size=1) +  xlab("time (month)") 


grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
