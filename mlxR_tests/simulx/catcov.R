
#-------------------------------------
p <- c(p_F=0.4,k_pop=0.2, omega_k=0.1, beta_F=0.6)
f <- list(name='f', time=seq(0, 30, by=0.1))
ind <- list(name=c("gender","k"))

res1 <- simulx(model    = "model/catcov1A.txt",
               parameter= p,
               output   = list(ind, f),
               group    = list(size=12,level='covariate'), 
               settings = list(seed=12345, load.design=TRUE))

print(res1$parameter)

fg<-merge(res1$f,res1$gender)
plot(ggplotmlx(data=fg) + geom_line(aes(x=time, y=f, group=id, colour=gender), size=0.5))

#-------------------------------------
res2 <- simulx(model    = "model/catcov1B.txt",
               parameter= p,
               output   = list(ind, f),
               group    = list(size=12,level='covariate'), 
               settings = list(seed=12345, load.design=TRUE))

print(res2$parameter)

fg<-merge(res2$f,res2$gender)
plot(ggplotmlx(data=fg) + geom_line(aes(x=time, y=f, group=id, colour=gender), size=0.5))

#-------------------------------------
p   <- c(w_pop=70, omega_w=15, p_A=0.2, p_B=0.4, k_pop=0.2, omega_k=0.05)
f   <- list(name='f', time=seq(0, 30, by=0.1))
ind <- list(name=c('w','k','trt'))

res3 <- simulx(model     = "model/catcov2A.txt",
               parameter = p,
               output    = list(ind, f),
               group     = list(size=200, level='covariate'), 
               settings  = list(seed=12345))

print(res3$parameter[1:10,])

fg<-merge(res3$f,res3$trt)
plot(ggplotmlx(data=fg) + geom_line(aes(x=time, y=f, group=id, colour=trt), size=0.5))

#-------------------------------------
p.indiv <- inlineDataFrame("
id   w    trt
 1  80     TA
 2  60     TB
 3  85     TC
 4  55     TA
 5  90     TB
 6  60     TC
")
p.pop <- c(k_pop=0.2, omega_k=0.1)

f <- list(name='f', time=seq(0, 30, by=0.1))
ind <- list(name=c('w','k','trt'))

res4 <- simulx(model     = "model/catcov2B.txt",
               parameter = list(p.indiv, p.pop),
               output    = list(ind, f),
               settings  = list(seed=123))

print(res4$parameter)
plot(ggplotmlx(data=res4$f, aes(x=time, y=f, colour=id)) + geom_line(size=0.75))



