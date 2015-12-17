setwd(dirname(parent.frame(2)$ofile))

#-------------------------------------

f <- list( name = 'f',
           time = seq(0, 1, by=0.1))

p <- c(u=50,v=10)

res <- simulx( model     = 'intro1_model.txt',
               parameter = p,
               output    = f)


pl=ggplotmlx(data=res$f, aes(x=time, y=f)) + geom_line(size=1)
print(pl)


