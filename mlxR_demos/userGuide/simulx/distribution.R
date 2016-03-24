update_geom_defaults("bar",   list(fill = "blue"))

N <- 10000

#--------------------------
normal.dist = inlineModel("
[LONGITUDINAL]
DEFINITION:
y1 = {distribution=normal, mean=3, sd=2}
y2 = {distribution=lognormal, mean=5, sd=0.3}
y3 = {distribution=logitnormal, mean=0.4, sd=0.5}
y4 = {distribution=probitnormal, mean=0.7, sd=1.5}
")

y <- list(name=c('y1','y2','y3','y4'),time=(1:N))
normal.res <- simulx(model = normal.dist, output=y)

pl1 <- ggplotmlx() + geom_histogram(data=normal.res$y1,aes(y1), bins=30)
pl2 <- ggplotmlx() + geom_histogram(data=normal.res$y2,aes(y2), bins=30)
pl3 <- ggplotmlx() + geom_histogram(data=normal.res$y3,aes(y3), bins=30)
pl4 <- ggplotmlx() + geom_histogram(data=normal.res$y4,aes(y4), bins=30)
grid.arrange(pl1, pl2, pl3, pl4)

z1 <- normal.res$y1$y1
print(c(mean(z1), sd(z1)))
z2 <- log(normal.res$y2$y2)
print(c(mean(z2), sd(z2)))
z3 <- log(normal.res$y3$y3/(1-normal.res$y3$y3))
print(c(mean(z3), sd(z3)))
z4 <- qnorm(normal.res$y4$y4)
print(c(mean(z4), sd(z4)))

#--------------------------
continuous.dist = inlineModel("
[LONGITUDINAL]
DEFINITION:
y5 = {distribution=uniform, min=-2.4, max=13}
y6 = {distribution=exponential, rate=0.7}
y7 = {distribution=gamma, shape=2, scale=0.5}
y8 = {distribution=weibull, shape=2, scale=1.2}
y9 = {distribution=extremeValue, location=-10, scale=1.8}
y10 = {distribution=chiSquared, df=2}
y11 = {distribution=cauchy, location=0, scale=1}
y12 = {distribution=fisherF, df1=40, df2=20}
y13 = {distribution=studentT, df=10}
")

y <- list(name=c('y5','y6','y7','y8','y9','y10','y11','y12','y13'),time=(1:N))
continuous.res <- simulx(model = continuous.dist, output=y)

pl5  <- ggplotmlx() + geom_histogram(data=continuous.res$y5,aes(y5), bins=30)
pl6  <- ggplotmlx() + geom_histogram(data=continuous.res$y6,aes(y6), bins=30)
pl7  <- ggplotmlx() + geom_histogram(data=continuous.res$y7,aes(y7), bins=30)
pl8  <- ggplotmlx() + geom_histogram(data=continuous.res$y8,aes(y8), bins=30)
pl9  <- ggplotmlx() + geom_histogram(data=continuous.res$y9,aes(y9), bins=30)
pl10 <- ggplotmlx() + geom_histogram(data=continuous.res$y10,aes(y10), bins=30)
pl11 <- ggplotmlx() + geom_histogram(data=continuous.res$y11,aes(log(abs(y11))), bins=30)
pl12 <- ggplotmlx() + geom_histogram(data=continuous.res$y12,aes(y12), bins=30)
pl13 <- ggplotmlx() + geom_histogram(data=continuous.res$y13,aes(y13), bins=30)
grid.arrange(pl5, pl6, pl7, pl8, pl9, pl10, pl11, pl12, pl13)

#--------------------------
discrete.dist = inlineModel("
[LONGITUDINAL]
DEFINITION:
y14 = {distribution=bernoulli, prob=0.3}
y15 = {distribution=discreteUniform, min=-4, max=2}
y16 = {distribution=binomial, size=30, prob=0.7}
y17 = {distribution=geometric, prob=0.2}
y18 = {distribution=negativeBinomial, size=3, prob=0.3}
y19 = {distribution=poisson, lambda=2}
")

y <- list(name=c('y14','y15','y16','y17','y18','y19'),time=(1:N))
discrete.res <- simulx(model = discrete.dist, output=y)

pl14 <- ggplotmlx() + geom_bar(data=discrete.res$y14,aes(y14))
pl15 <- ggplotmlx() + geom_bar(data=discrete.res$y15,aes(y15))
pl16 <- ggplotmlx() + geom_bar(data=discrete.res$y16,aes(y16))
pl17 <- ggplotmlx() + geom_bar(data=discrete.res$y17,aes(y17))
pl18 <- ggplotmlx() + geom_bar(data=discrete.res$y18,aes(y18))
pl19 <- ggplotmlx() + geom_bar(data=discrete.res$y19,aes(y19))
grid.arrange(pl14, pl15, pl16, pl17, pl18, pl19)

