# setwd(dirname(parent.frame(2)$ofile))
library(gridExtra)
source('titration.R')

#-------------------------------------

adm <- list(time=seq(0,to=440,by=12), amount=20)

ppk <- c(ka_pop=0.4,   V_pop=10,    k_pop=0.05,
         omega_ka=0.3, omega_V=0.5, omega_k=0.1, a1=0.05)

g   <- list(size=10, level='individual')

Cc  <- list(name='Cc', time=seq(0,to=440,by=1))
y1  <- list(name='y1', time=seq(4,to=440,by=24))

s   <- list(seed=1234)

res1 <- simulx(model     = "model/cts1a.txt",
               parameter = ppk,
               treatment = adm,
               group     = g,
               output    = list(Cc, y1),
               settings  = s)

plot1 <- ggplotmlx(data=res1$Cc, aes(x=time, y=Cc, colour=id)) +  
  geom_line(size=0.5) + xlab("time (hour)") + ylab("Cc (mg/l)") +
  theme(legend.position="none")
plot2 <- ggplotmlx(data=res1$y1, aes(x=time, y=y1, colour=id)) +  
  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured concentration (mg/l)") + 
  theme(legend.position="none")
grid.arrange(plot1, plot2, ncol=2)

#--------------------------------------------------------
r1 <- list(name='y1', 
           time=seq(124,to=440,by=24), 
           condition="y1>5", 
           factor=0.75)

res2 <- titration(model     = "model/cts1a.txt",
                  parameter = ppk,
                  treatment = adm,
                  output    = list(Cc, y1),
                  rule      = r1,
                  group     = g,
                  settings  = s)

plot3 <- ggplotmlx(data=res2$Cc, aes(x=time, y=Cc, colour=id)) +  
  geom_line(size=0.5) + xlab("time (hour)") + ylab("Cc (mg/l)") +
  theme(legend.position="none")  + geom_hline(yintercept=5)
plot4 <- ggplotmlx(data=res2$y1, aes(x=time, y=y1, colour=id)) +  
  geom_line(size=0.5) + geom_point(size=2) + 
  xlab("time (hour)") + ylab("measured concentration (mg/l)") + 
  theme(legend.position="none") + geom_hline(yintercept=5)
grid.arrange(plot3, plot4, ncol=2)

#--------------------------------------------------------
r2 <- list(name='y1', 
           time=seq(124,to=440,by=24),
           condition="y1<3", 
           factor=1.5)

res3 <- titration(model     = "model/cts1a.txt",
                  parameter = ppk,
                  treatment = adm,
                  output    = list(Cc, y1),
                  rule      = list(r1,r2),
                  group     = g,
                  settings  = s)

plot5 <- ggplotmlx(data=res3$Cc, aes(x=time, y=Cc, colour=id)) +  
  geom_line(size=0.5) + xlab("time (hour)") + ylab("Cc (mg/l)") +
  theme(legend.position="none") + geom_hline(yintercept=c(3,5))
plot6 <- ggplotmlx(data=res3$y1, aes(x=time, y=y1, colour=id)) +  
  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured concentration (mg/l)") + 
  theme(legend.position="none") + geom_hline(yintercept=c(3,5))
grid.arrange(plot5, plot6, ncol=2)

#--------------------------------------------------------
ppd <- c(Emax_pop=100,   EC50_pop=3, 
         omega_Emax=0.1, omega_EC50=0.2, a2=5)

E <-  list(name='E',  time=seq(0,to=440,by=1))
y2 <- list(name='y2', time=seq(16,to=440,by=36))

res4 <- simulx(model     = "model/cts1b.txt",
               parameter = c(ppk,ppd),
               treatment = adm,
               output    = list(Cc, E, y1, y2),
               group     = g,
               settings  = s)

pl1=ggplotmlx(data=res4$Cc, aes(x=time, y=Cc, colour=id)) +  geom_line(size=0.5) +
  xlab("time (hour)") + ylab("Cc (mg/l)") + theme(legend.position="none")
pl2=ggplotmlx(data=res4$E, aes(x=time, y=E, colour=id)) +  geom_line(size=0.5) +
  xlab("time (hour)") + ylab("E") + theme(legend.position="none")
pl3=ggplotmlx(data=res4$y1, aes(x=time, y=y1, colour=id)) +  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured concentration (mg/l)") + theme(legend.position="none")
pl4=ggplotmlx(data=res4$y2, aes(x=time, y=y2, colour=id)) +  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured effect") + theme(legend.position="none")

grid.arrange(pl1, pl2, pl3, pl4, ncol=2)

#--------------------------------------------------------
r3 <- list(name='y2', 
           time=seq(88,to=440,by=36),
           condition="y2<40", 
           factor=1.5)

res5 <- titration(model     = "model/cts1b.txt",
                  parameter = c(ppk,ppd),
                  treatment = adm,
                  output    = list(Cc, E, y1, y2),
                  rule      = list(r1,r3),
                  group     = g,
                  settings  = s)

pl5=ggplotmlx(data=res5$Cc, aes(x=time, y=Cc, colour=id)) +  geom_line(size=0.5) +
  xlab("time (hour)") + ylab("Cc (mg/l)") + theme(legend.position="none")  +
  geom_hline(yintercept=5)
pl6=ggplotmlx(data=res5$E, aes(x=time, y=E, colour=id)) +  geom_line(size=0.5) +
  xlab("time (hour)") + ylab("E") + theme(legend.position="none")  +
  geom_hline(yintercept=40)
pl7=ggplotmlx(data=res5$y1, aes(x=time, y=y1, colour=id)) +  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured concentration (mg/l)") + theme(legend.position="none")  +
  geom_hline(yintercept=5)
pl8=ggplotmlx(data=res5$y2, aes(x=time, y=y2, colour=id)) +  geom_line(size=0.5) + geom_point(size=2) +
  xlab("time (hour)") + ylab("measured effect") + theme(legend.position="none")  +
  geom_hline(yintercept=40)

grid.arrange(pl5, pl6, pl7, pl8, ncol=2)
