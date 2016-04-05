
#---------------------------------------------
op <- list(name= c('a','b','c'))
of <- list(name='f', time=seq(0,4, by=0.1))

p <- c(a_pop=10,   b_pop=-5,   c_pop=0.8, 
       o_a  =0.3,  o_b  =0.5,  o_c  =0.4, 
       r_ab =-0.6, r_ac =-0.4, r_bc =0.7)

g <- list(size=100, level='individual')

res <- simulx(model     = "model/correlation1.txt",
              parameter = p, 
              group     = g, 
              output    = list(op,of))

p=res$parameter[,2:4]
z=p
z[,"a"]=log(p[,"a"])
z[,"c"]=log(p[,"c"]/(1-p[,"c"]))
print(sapply(p,median))
print(sapply(z,sd))
print(cor(z))

#---------------------------------------------
op <- list(name= c('a_pred','a','b','w','h'))
of <- list(name='f', time=seq(0,4, by=0.1))

p <- c(a_pop=10,   b_pop=-5,   w_pop=70, h_pop=170,
       o_a  =0.3,  o_b  =0.5,  o_w  =10, o_h  =10, 
       r_ab =-0.6, r_wh =0.8)

g <- list(size=100, level='covariate')

res <- simulx(model     = "model/correlation2.txt",
              parameter = p, 
              group     = g, 
              output    = list(op,of))

p=res$parameter[,2:6]
print(cor(p[,2:5]))  #correlation between a & b

p[,"a"]=p[,"a"] - p[,"a_pred"]
print(cor(p[,2:5]))  #conditional correlation between a & b (given w & h)
