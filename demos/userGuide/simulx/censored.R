
#-------------------------------------
myModel <- inlineModel("
[LONGITUDINAL]
input = {ka, V, k, Emax, EC50, b1, b2}

EQUATION:
D = 100
f1 = D*ka/(V*(ka-k))*(exp(-k*t) - exp(-ka*t))
f2 = Emax*f1/(f1 + EC50)
g1 = b1*f1
g2 = b2*f2

DEFINITION:
y1 = {distribution=normal, prediction=f1, sd=g1}                      
y2 = {distribution=normal, prediction=f2, sd=g2}                      
")


f <- list(name= c('f1','f2'), time=seq(0, 30, by=0.1))
y <- list(name=c('y1','y2'), time=seq(0, 30, by=2))
p <- c(ka=0.5, V=10, k=0.2, Emax=100, EC50=1.5, b1=0.2, b2=0.15)
s <- 123456

res1a <- simulx(model   = myModel, 
                parameter = p, 
                output    = list(f, y),
                settings  = list(seed=s) )

pla1 <- ggplotmlx() + geom_line(data=res1a$f1, aes(x=time, y=f1), size=0.5) +
  geom_point(data=res1a$y1, aes(x=time, y=y1), colour="red") + ylim(c(0, 6))
pla2 <- ggplotmlx() + geom_line(data=res1a$f2, aes(x=time, y=f2), size=0.5) +
  geom_point(data=res1a$y2, aes(x=time, y=y2), colour="red") + ylim(c(0, 90))
grid.arrange(pla1, pla2, ncol=2)


y1 <- list(name='y1', time=seq(0, 30, by=2), limit=0, lloq=0.8)
y2 <- list(name='y2', time=seq(0, 30, by=2), uloq=60)

res1b <- simulx(model   = myModel, 
                parameter = p, 
                output    = list(y1, y2),
                settings  = list(seed=s) )


plb1 <- ggplotmlx() + geom_line(data=res1a$f1, aes(x=time, y=f1), size=0.5) +
  geom_point(data=res1b$y1, aes(x=time, y=y1, colour=cens)) +
  theme(legend.position=c(.8, .8)) + ylim(c(0, 6))
plb2 <- ggplotmlx() + geom_line(data=res1a$f2, aes(x=time, y=f2), size=0.5) +
  geom_point(data=res1b$y2, aes(x=time, y=y2, colour=cens))+
  theme(legend.position=c(.8, .8)) + ylim(c(0, 90))
grid.arrange(plb1, plb2, ncol=2)

####

writeDatamlx(res1b, result.folder = "res1b")

writeDatamlx(res1b, result.file = "res1b.csv")
head(read.table("res1b.csv", header=T, sep=","))


res1c <- simulx(model     = myModel, 
                parameter = p, 
                result.file   ="res1c.csv",
                output    = list(y1, y2),
                settings  = list(seed=s))

head(read.table("res1c.csv", header=T, sep=","))

