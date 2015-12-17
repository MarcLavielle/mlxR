setwd(dirname(parent.frame(2)$ofile))
library(ggplot2)

#-------------------------------------

adm <- list(colNames=c("id","time","amount","rate"), 
            value=matrix(ncol=4,nrow=6,byrow=TRUE,
            data=c(   1,     1,   100,  0,
                      1,    12,    50,  10, 
                      1,    24,   100,  10,
                      2,     6,    75,  15,
                      2,    18,   100,  0,
                      2,    24,    75,  15 )))
p1 <- list(name=c('V_pop','omega_V','a'),value=c(10,0.3,1)) 
p2 <- list(name=c("w","k"), colNames=c("id","w","k"), 
          value=matrix(ncol=3,nrow=4,byrow=TRUE,
          data=c(     1,    75,   0.5,
                      2,    60,   0.4 )))
p2 <- inlineDataFrame("
 id    w     k
 1    75   0.5
 2    60   0.4                     
")
                  
# p2 <- list(name=c("w","k"), 
#           value=matrix(ncol=2,nrow=4,byrow=TRUE,
#           data=c(      75,   0.5,
#                        60,   0.4 )))
p <- list(p1, p2)
out1 <- list(name="Cc", time=seq(0, 50, by=1))
out2 <- list(name="y", colNames=c("id","time"), 
             value=matrix(ncol=2,nrow=7,byrow=TRUE,
             data=c(  1,     3,  
                      1,    15,  
                      1,    27,  
                      1,    30,  
                      2,     2,  
                      2,    18,  
                      2,    24 )))
out <- list(out1, out2)

res <- simulx(model="group2_model.txt", parameter=p, output=out, treatment=adm)
                            
plot1 <- ggplotmlx(data=res$Cc, aes(x=time, y=Cc, colour=id)) + geom_line(size=1) +
         geom_point(data=res$y, aes(x=time, y=y, colour=id)) 
print(plot1)
