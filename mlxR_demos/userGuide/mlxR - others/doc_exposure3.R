library(mlxR)
library(gridExtra)


myModel <- inlineModel("
[LONGITUDINAL] 
input={ka,V,k,Imax,S0,IC50,kout}

EQUATION:
Cc  = pkmodel(ka, V, k)
Ec = Imax*Cc/(Cc+IC50)
PCA_0 = S0 
ddt_PCA = kout*((1-Ec)*S0- PCA)  

[INDIVIDUAL]
input={ka_pop,omega_ka,V_pop,omega_V,beta_V,k_pop,omega_k,beta_k,
       Imax_pop,omega_Imax,S0_pop,omega_S0,
       IC50_pop,omega_IC50,kout_pop,omega_kout,w,w_pop}

EQUATION:
V_pred = V_pop*(w/w_pop)^beta_V
k_pred = k_pop*(w/w_pop)^beta_k

DEFINITION:
ka  ={distribution=lognormal, prediction=ka_pop,   sd=omega_ka}
V   ={distribution=lognormal, prediction=V_pred,   sd=omega_V}
k   ={distribution=lognormal, prediction=k_pred,   sd=omega_k}
S0  ={distribution=lognormal, prediction=S0_pop,   sd=omega_S0}
IC50={distribution=lognormal, prediction=IC50_pop, sd=omega_IC50}
kout={distribution=lognormal, prediction=kout_pop, sd=omega_kout}
Imax={distribution=logitnormal, prediction=Imax_pop, sd=omega_Imax}

[COVARIATE]
input={w_pop, omega_w}

DEFINITION:
w={distribution=normal, mean=w_pop, sd=omega_w}
")


p <- c(w_pop=70,     omega_w=10, 
       ka_pop=0.5,   omega_ka=0.2, 
       V_pop=10,     omega_V=0.1,    beta_V= 1,
       k_pop=0.1,    omega_k=0.15,   beta_k=-0.25,
       Imax_pop=0.8, omega_Imax=0.4,
       S0_pop=100,   omega_S0=0, 
       IC50_pop=1,   omega_IC50=0.2,
       kout_pop=0.1, omega_kout=0.1)

N <- 1000

adm1 <- list(time=seq(0, 240, by=24), amount=100)
adm2 <- list(time=seq(0, 252, by=12), amount=50)
g1 <- list(treatment=adm1,size=N,level='covariate')
g2 <- list(treatment = adm2,size=N,level='covariate')
out <- list(name=c("Cc","PCA"), time=seq(240,264,length=100))

res <- exposure(model     = myModel, 
                parameter = p, 
                output    = out,
                group     = list(g1,g2))

b11 <- ggplotmlx(data=res$Cc)+geom_boxplot(aes(x=group,y=auc))
b12 <- ggplotmlx(data=res$Cc)+geom_boxplot(aes(x=group,y=cmax))
grid.arrange(b11,b12,nrow=1)

b21 <- ggplotmlx(data=res$PCA)+geom_boxplot(aes(x=group,y=cmin))
b22 <- ggplotmlx(data=res$PCA)+geom_boxplot(aes(x=group,y=cmax))
grid.arrange(b21,b22,nrow=1)

Cc  <- res$output$Cc
p11 <- prctilemlx(r=Cc[Cc$group==1,])  +  theme(legend.position="none") + ylim(c(0,10)) + ggtitle("group 1")
p12 <- prctilemlx(r=Cc[Cc$group==2,])  +  theme(legend.position="none") + ylim(c(0,10)) + ggtitle("group 2")
grid.arrange(p11,p12,nrow=1)

PCA <- res$output$PCA
p21 <- prctilemlx(r=PCA[PCA$group==1,]) + theme(legend.position="none") + ylim(c(20,60))+ ggtitle("group 1")
p22 <- prctilemlx(r=PCA[PCA$group==2,]) + theme(legend.position="none") + ylim(c(20,60))+ ggtitle("group 2")
grid.arrange(p21,p22,nrow=1)
