#------------------------------------------------------
model.pharmml <- 'pharmML/Rocchetti_2013_oncology_TGI_antiangiogenic_combo_v1.xml'
#------------------------------------------------------

d = read.csv('data/rocchetti2013_data.csv',skip=1,na='.')
head(d)

adm1 <- list( time  = d$TIME[d$EVID==1&d$CMT==1],
             amount = d$AMT[d$EVID==1&d$CMT==1],
             target = 'Q0_A')

adm2 <- list( time   = d$TIME[d$EVID==1&d$CMT==3],
              amount = d$AMT[d$EVID==1&d$CMT==3],
              target = 'Q1_B')

p <- c(Emax=1, FV1_A=1/0.119, FV1_B=1/2.13, IC50=3.6, IC50combo=2.02,
       k1=3.54, k12=141.1, k2=0.221, k21=10.4,
       ka_A=24*log(2)/6.19, ka_B=18.8, ke_A=log(2)/6.05, ke_B=49.2,
       lambda0=0.14, lambda1=0.129, psi=20, CV=0.1, w0=0.062)

out   <- list( name = c('Wtot','y'), time = d$TIME[d$EVID!=1])

res <- simulx( model     = model.pharmml,
               parameter = p,
               treatment = list(adm1, adm2),
               output    = out,
               settings  = list(seed=12345))

print(ggplotmlx() + geom_line(data=res$Wtot, aes(x=time, y=Wtot), colour="black") + 
                 geom_point(data=res$y, aes(x=time, y=y), colour="red"))
