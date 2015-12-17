library(gridExtra)
# # library("mlxR")

model.mlxtran <- 'Mlxtran/ribba1.txt'

adm1 <- list( time=seq(0,7.5,by=1.5), amount=1)
adm2 <- list(time=seq(0, 45, by= 9),  amount=1)

f <- list(name=c('PT','Q','QP','PSTAR'), time=seq(-50,100,by=0.5))

psi <- c(K=100, KDE=0.3, KPQ=0.025, KQPP=0.004, LAMBDAP=0.12, GAMMA=1, DELTAQP=0.01)

g1 <- list( treatment = adm1)
g2 <- list( treatment = adm2)

res <- simulx(model     = model.mlxtran,
              parameter = psi,
              output    = f,
              group     = list(g1,g2))

t1=adm1$time[length(adm1$time)]
t2=adm2$time[length(adm2$time)]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]}

vl1 <- geom_vline(xintercept=c(t1,t2),color=gg_color_hue(2),size=0.25) 

plot1=ggplotmlx(data=res$PT, aes(x=time, y=PT, colour=id)) +  geom_line(size=1) +
  xlab("time (month)")  + theme(legend.position="none") + vl1
plot2=ggplotmlx(data=res$Q,  aes(x=time, y=Q, colour=id)) +  geom_line(size=1) +
  xlab("time (month)") + vl1 
plot3=ggplotmlx(data=res$QP, aes(x=time, y=QP, colour=id)) + geom_line(size=1) +
  xlab("time (month)") + theme(legend.position="none") + vl1 
plot4=ggplotmlx(data=res$PSTAR, aes(x=time, y=PSTAR, colour=id)) +
  geom_line(size=1) +  xlab("time (month)") + vl1
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


