
#-------------------------------------
myModel = inlineModel("
[LONGITUDINAL]
input =  {a, b, s}
EQUATION:
f = a + b*t
DEFINITION:
y = {distribution=normal, prediction=f, sd=s}
")

res <- simulx(model     = myModel,
              parameter = c(a=10, b=10, s=0.5),
              settings  = list(seed=12345),
              output    = list(name='y',time=(1:5)))
print(res$y)

res <- simulx(model     = myModel,
              parameter = c(a=10, b=10, s=0.5),
              settings  = list(seed=12345),
              output    = list(name='y',time=(1:5)))
print(res$y)

res <- simulx(model     = myModel,
              parameter = c(a=10, b=10, s=0.5),
              settings  = list(seed=54321),
              output    = list(name='y',time=(1:5)))
print(res$y)


