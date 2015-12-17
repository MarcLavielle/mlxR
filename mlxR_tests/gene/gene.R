library(reshape)
# # library("mlxR")

valveModel <- inlineModel("
[LONGITUDINAL] 
PK:
depot(target=v)
EQUATION:
ddt_v=0 
")

ton  <- list(amount = 1,  time   = c(10, 35))
toff <- list(amount = -1, time   = c(18, 50))
out  <- list(name = 'v', time = seq(0, 70, by=0.1))

res <- simulx(model=valveModel, output=out, treatment=list(ton,toff))
print(ggplotmlx(res$v) + geom_line(aes(time,v)) + ylab('valve status'))

#----------------------------------------------------------------------
out  <- list(name = c('h','s','u'), 
             time = seq(0, 70, by=0.1))

p1    <- c(kappa=0.3, gamma=0.7, nh=6, Kd=0.15, s0=0.016) 

res <- simulx(model     = 'model/hog1_model.txt', 
              parameter = p1, 
              output    = out, 
              treatment = list(ton, toff))

r <- merge(res$h,merge(res$s,res$u))
r <- melt(r, id = 'time', variable_name = 'signal')
print(ggplotmlx(r, aes(time,value)) + geom_line(aes(colour = signal),size=1) +
        ylab('activation')+ theme(legend.position=c(.9, .7)))


#----------------------------------------------------------------------
ton  <- list(amount = 1, rate   = 1, 
             time   = c(35, 65, 95, 125, 155, 185, 215, 245, 280, 
                        341, 371, 401, 449, 479, 533, 563, 649, 683))

toff <- list(amount = -1, rate   = 0.25, 
             time   = c(43, 73, 103, 133, 163, 193, 223, 253, 285, 
                        349, 379, 409, 454, 486, 541, 570, 657, 691))

out  <- list(name=c('x6'),  time=seq(0, 800, by=0.5))

p2 <- c(c1=75, c2=2500, c3=0.00002, c4=0.04, c5=1.2, c6=800, c7=0.005, c8=0.1, x30=150, tau=10) 

res <- simulx(model     = 'model/hog2_model.txt', 
              parameter = c(p1, p2), 
              output    = out, 
              treatment = list(ton, toff))

plot(ggplotmlx() + geom_line(data=res$x6, aes(x=time, y=x6)))




