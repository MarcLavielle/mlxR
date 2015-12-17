setwd(dirname(parent.frame(2)$ofile))


p <- list(name=c('ka','V','k'), value=c(0.5,10,0.2))
f1 <- list(name='f1', time=seq(5, 30, by=0.5))
f2 <- list(name='f2', time=seq(0, 15, by=0.1))
f <- list(f1, f2)
res <- simulx(model='prediction_model.txt', parameter=p, output=f)

print(ggplotmlx() + 
geom_line(data=res$f1, aes(x=time, y=f1,color="black")) +
geom_line(data=res$f2, aes(x=time, y=f2,color="red"  )) +
scale_colour_manual(name="", values =c('black','red'), labels = c('f1','f2')) +
ylab("concentration")  )

#+ theme(legend.position=c(0.9,0.5)) 