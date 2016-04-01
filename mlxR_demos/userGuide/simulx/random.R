# library(mlxR)

#-------------------------------------
myModel1 = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t
e ~ normal(0,s)
y = f*exp(e)
")

f <- list(name='f',time=seq(0, 100, by=1))
y <- list(name='y',time=seq(5, 100, by=10))
e <- list(name='e',time=seq(5, 100, by=10))
p <- c(a=10, b=0.5, s=0.2)
s <- list(seed=12345)

res <- simulx(model     = myModel1,
              parameter = p,
              settings  = s,
              output    = list(f, y, e))

pl1 <- ggplotmlx() + 
  geom_point(aes(x=time, y=y), data=res$y, color="red", size=3) +
  geom_line(aes(x=time, y=f), data=res$f, size=1) 

pl2 <- ggplotmlx(aes(x=time, y=e), data=res$e) +  geom_point(color="red", size=3) +
  geom_hline(yintercept = 0)

gridExtra::grid.arrange(pl1,pl2)

print(head(res$y))

#-------------------------------------
myModel2 = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t

DEFINITION:
y = {distribution=lognormal, prediction=f, sd=s}                      
")
res <- simulx(model     = myModel2,
              parameter = p,
              settings  = s,
              output    = list(f, y))

print(head(res$y))


#-------------------------------------
myModel3 = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t
e ~ normal(0,s)
y = f*exp(e)

[INDIVIDUAL]
input = {a_pop, omega_a, b_pop, omega_b}
EQUATION:
a ~ normal(a_pop,omega_a)
b ~ normal(b_pop,omega_b)
")

i <- list(name=c('a', 'b'))

res <- simulx(model     = myModel3,
              parameter = c(a_pop=10, omega_a=1, b_pop=0.5, omega_b=0.1, s=0.2),
              settings  = list(seed=12345),
              output    = list(y, i))

print(res$parameter)
print(head(res$y))

#-------------------------------------
myModel4 = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}
EQUATION:
f = a + b*t
DEFINITION:
y = {distribution=lognormal, prediction=f, sd=s}                      

[INDIVIDUAL]
input = {a_pop, omega_a, b_pop, omega_b}
DEFINITION:
a = {distribution=normal, mean=a_pop, sd=omega_a}                      
b = {distribution=normal, mean=b_pop, sd=omega_b}                      
")

res <- simulx(model     = myModel4,
              parameter = c(a_pop=10, omega_a=1, b_pop=0.5, omega_b=0.1, s=0.2),
              settings  = list(seed=12345),
              output    = list(y, i))

print(res$parameter)
print(head(res$y))




