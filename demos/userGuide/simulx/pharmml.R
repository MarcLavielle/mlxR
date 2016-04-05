library("gridExtra")

#-------------------------------------
pharmML.model <- "pharmML/pk1.xml"
# mlxtran.model <- "pharmML/pk1.txt"

mlxtran.model <- pharmml2mlxtran(pharmML.model)

adm <- list( time   = 4, amount = 100)

p <- c(w=70, pop_Tlag=1, omega_Tlag=0.3, pop_ka=0.8, omega_ka=0.2, pop_V=10, omega_V=0.3, 
       beta_V=1, pop_Cl=1, omega_Cl=0.2, beta_Cl=0.75, a=0, b=0.1)

ind <- list( name = c('Tlag','ka','V','Cl'))
f   <- list( name = c('Cc'), time = seq(0,to=60,by=1))
y   <- list( name = c('C_obs'), time = seq(2,to=50,by=2))

g <- list( size = 4, level = 'individual',  treatment = adm);

res <- simulx( model     = pharmML.model,
               parameter = p,
               output    = list(ind, f, y),
               group     = g);

print(res$parameter)

plot(ggplotmlx() + 
  geom_line(data=res$Cc, aes(x=time, y=Cc, colour=id)) +
  geom_point(data=res$C_obs, aes(x=time, y=C_obs, colour=id)) +
  xlab("time (h)") + ylab("concentration") )

#-------------------------------------
pharmML.model <- "pharmML/pkpd1.xml"

adm1 <- list(time=seq(24,to=143,by=24),amount=100)
adm2 <- list(time=seq(24,to=143,by=12),amount= 50)

p <- list(name=c('w','pop_ka','omega_ka','pop_V','omega_V',
                 'beta_V','pop_Cl','omega_Cl','beta_Cl',
                 'pop_Imax','omega_Imax','pop_IC50','omega_IC50',
                 'pop_Rin','omega_Rin','pop_kout','omega_kout','a1','b1','a2'), 
          value=c(70, 0.8, 0.2, 10, 0.3, 1, 1, 0.2, 0.75, 0.8, 0.4, 3, 0.1, 10, 
                  0.1, 0.1, 0.1, 0.5, 0.1, 2,0.1,0.2,0.3))

ind <- list(name=c('ka','V','Cl','Imax','IC50','Rin','kout'))
f <- list(name=c('Cc','PCA'), time=seq(0,to=200,by=1))
y <- list(name=c('Cc_obs','PCA_obs'), time=seq(1,to=200,by=5))
out <- list(ind, f, y)

g1 <- list(size=4, level='individual', treatment=adm1);
g2 <- list(size=4, level='individual', treatment=adm2);
g <- list(g1,g2)

res <- simulx(model=pharmML.model,parameter=p,output=out,group=g)

plot1=ggplotmlx(data=res$Cc, aes(x=time, y=Cc, group=id, colour=group)) +  geom_line(size=0.5) +
  xlab("time (h)") + ylab("predicted concentration (mg/l") + theme(legend.position="none")
plot2=ggplotmlx(data=res$PCA, aes(x=time, y=PCA, group=id, colour=group)) +  geom_line(size=0.5) +
  xlab("time (h)") + ylab("predicted PCA") 
plot3=ggplotmlx(data=res$Cc_obs, aes(x=time, y=Cc_obs, group=id, colour=group)) +  geom_point() +
  xlab("time (h)") + ylab("concentration (mg/l") + theme(legend.position="none")
plot4=ggplotmlx(data=res$PCA_obs, aes(x=time, y=PCA_obs, group=id, colour=group)) +  geom_point() +
  xlab("time (h)") + ylab("PCA") 
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

print(res$parameter)
