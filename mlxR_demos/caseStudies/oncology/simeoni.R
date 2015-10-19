#------------------------------------------------------
model.pharmml <- "pharmML/Simeoni_2004_oncology_TGI_v2.xml"
#------------------------------------------------------

d = read.csv('data/simeoni2004_data.csv',skip=1,na='.')
head(d)

p <- c(V1=0.81,  k1=0.968, k2=0.629,  
       k10=0.868*24, k12=0.006*24, k2=0.629, k21=0.0838*24,
       lambda0=0.273, lambda1=0.814, psi=20, CV=0.1, w0=0.055)

adm2 <- list( time  = d$TIME[d$EVID==1], 
              amount = d$AMT[d$EVID==1], 
              target = 'Q1')

f1 <- list( name='Wtot', time=seq(0,30,by=0.5))
f2 <- list( name='Wtot', time=seq(0,45,by=0.5))
y1 <- list( name = 'y', time = d$TIME[d$EVID!=1&d$ID==1])
y2 <- list( name = 'y', time = d$TIME[d$EVID!=1&d$ID==2])

g1 <- list( output = list(y1, f1))
g2 <- list( treatment = adm2, output = list(y2, f2))

res <- simulx( model     = model.pharmml,
               parameter = p,
               group     = list(g1,g2),
               settings  = list(seed=12345) )

print(ggplotmlx() + geom_line(data=res$Wtot, aes(x=time, y=Wtot, colour=id)) + 
  geom_point(data=res$y, aes(x=time, y=y,colour=id)))

