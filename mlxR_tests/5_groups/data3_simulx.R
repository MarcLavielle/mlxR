setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(header=c("id","time","amount","rate"), 
            value=matrix(ncol=4,nrow=6,byrow=TRUE,
            data=c(     1,     1,   100,  0,
                        1,    12,    50,  10, 
                        1,    24,   100,  10,
                        2,     6,    75,  15,
                        2,    18,   100,  0,
                        2,    24,    75,  15 )))
p <- list(name=c("V","k"),value=c(10,0.15)) 
Cc <- list(name="Cc", time=seq(0, 50, by=1))
res <- simulx(model="group4_model.txt", parameter=p, output=Cc, treatment=adm)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1))

