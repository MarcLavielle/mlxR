
#-------------------------------------
myModel = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t
e ~ normal(0,s)
y = f*exp(e)

;DEFINITION:
;e = {distribution=normal, mean=0, sd=s}

")

f <- list(name='f',time=seq(0, 100, by=1))
y <- list(name='y',time=seq(5, 100, by=10))
p <- c(a=10, b=0.5, s=0.2)
s <- list(seed=12345)

res <- simulx(model     = myModel,
              parameter = p,
              settings  = s,
              output    = list(f, y))

print(ggplotmlx(aes(x=time, y=f), data=res$f) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res$y, color="red", size=3))


#-------------------------------------
myModel = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}

EQUATION:
f = a + b*t

DEFINITION:
y = {distribution=lognormal, prediction=f, sd=s}                      
")
res <- simulx(model     = myModel,
              parameter = p,
              settings  = s,
              output    = list(f, y))

print(ggplotmlx(aes(x=time, y=f), data=res$f) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res$y, color="red", size=3))

#-------------------------------------
myModel = inlineModel("
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
e <- list(name=c('e','y','f'),time=seq(5, 100, by=10))

res <- simulx(model     = myModel,
              parameter = c(a_pop=10, omega_a=1, b_pop=0.5, omega_b=0.1, s=0.2),
              settings  = list(seed=12345),
              output    = list(e, i))

print(res$parameter)

#print(cbind(exp(res$e$e)*res$f$f, res$y$y))
print(ggplotmlx(aes(x=time, y=f), data=res$f) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res$y, color="red", size=3))

#-------------------------------------
myModel = inlineModel("
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

res <- simulx(model     = myModel,
              parameter = c(a_pop=10, omega_a=1, b_pop=0.5, omega_b=0.1, s=0.2),
              settings  = list(seed=12345),
              output    = list(f, y, i))

print(res$parameter)
print(ggplotmlx(aes(x=time, y=f), data=res$f) + geom_line(size=1) +
        geom_point(aes(x=time, y=y), data=res$y, color="red", size=3))


#-------------------------------------
myModel = inlineModel("
[LONGITUDINAL]
                                            
EQUATION:
e ~ normal(0,1)
y = e
")

e <- list(name=c('e','y'),time=seq(5, 100, by=10))
res <- simulx(model = myModel, settings = list(seed=12345), output=e)
print(merge(res$e, res$y))

#-------------------------------------
myModel = inlineModel("
[LONGITUDINAL]
DEFINITION:
e = {distribution=poisson, lambda=2}                      
")

e <- list(name=c('e'),time=seq(5, 100, by=10))
res <- simulx(model = myModel, output=e)
print(res$e)

