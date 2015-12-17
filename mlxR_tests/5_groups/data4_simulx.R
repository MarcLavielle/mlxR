setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=0, amount=100)
y <- list(name="y", header=c("id","time"), 
            value=matrix(ncol=2,nrow=7,byrow=TRUE,
            data=c(     1,     3,  
                        1,     6,  
                        1,     9,  
                        1,    12,  
                        2,     2,  
                        2,    10,  
                        2,    18 )))
p <- list(name=c("V","k","a"),value=c(10,0.15,0.3)) 

res <- simulx(model="group1_model.txt", parameter=p, output=y, treatment=adm)

print(ggplotmlx(data=res$y, aes(x=time,y=y,color=id))+geom_line(size=1) + geom_point())
