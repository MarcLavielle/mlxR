setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=0, amount=100)
p <- list(name=c("V","k"), header=c("id","V","k"),
          value=matrix(ncol=3,nrow=4,byrow=TRUE,
                       data=c(     1,    12,   0.15,
                                   2,     9,   0.25,
                                   3,     8,   0.15,
                                   4,    11,   0.20 )))
Cc <- list(name="Cc", time=seq(0, 10, by=1))

res <- simulx(model="group4_model.txt", parameter=p, output=Cc, treatment=adm)

print(ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1))
