
myModel = inlineModel("
[INDIVIDUAL]
input = {a_pop, o_a, b_pop, o_b, c_pop, o_c, r_ab, r_ac, r_bc}

DEFINITION:
a = {distribution=lognormal,   reference=a_pop, sd=o_a}
b = {distribution=normal,      reference=b_pop, sd=o_b}
c = {distribution=logitnormal, reference=c_pop, sd=o_c}
correlation = {r(a,b)=r_ab, r(a,c)=r_ac, r(b,c)=r_bc}

[LONGITUDINAL]
input={a, b, c}
EQUATION:
f = a + b*t + c*t^2
")

op <- list(name= c('a','b','c'))
of <- list(name='f', time=seq(0,4, by=0.1))

p <- list(name=c('a_pop','o_a','b_pop','o_b','c_pop','o_c','r_ab','r_ac','r_bc'), 
          value=c(10, 0.3, -5, 0.5, 0.8, 0.4, -0.6, -0.4, 0.7))

g <- list(size=1000, level='individual')

res <- simulx(model=myModel,parameter=p, group=g, output=list(op,of))

p=res$parameter[,2:4]
z=p
z[,"a"]=log(p[,"a"])
z[,"c"]=log(p[,"c"]/(1-p[,"c"]))
print(sapply(p,median))
print(sapply(z,sd))
print(cor(z))
