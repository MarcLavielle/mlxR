setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(time=0, amount=100)
p <- list( list(name="V", header=c("id","V"), 
                value=matrix(ncol=2,nrow=4,,byrow=TRUE,
                             data=c(     1,    12, 
                                         2,     9,
                                         3,     8,
                                         4,    11 ))),
           list(name="k", value=0.2))
# p <- list( list(name="V", 
#                 value=matrix(ncol=1,nrow=4,,byrow=TRUE,
#                              data=c(         12, 
#                                              9,
#                                              8,
#                                             11 ))),
#            list(name="k", value=0.2))
Cc <- list(name="Cc", time=seq(0, 10, by=1))
res <- simulx(model="group4_model.txt", parameter=p, output=Cc, treatment=adm)

plot1 <- ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1) 
print(plot1)
