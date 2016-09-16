setwd(dirname(parent.frame(2)$ofile))

###
project1 <- 'monolixRuns/theophylline_project.mlxtran'

res0 <- simulx(project  = project1, 
               settings = list(seed=12321) )
head(res0$y1, n=9)

out.y <- list(name="y1", lloq=2)
res1 <- simulx(project  = project1, 
               output   = out.y,
               settings = list(seed=12321) )
head(res1$y1, n=10)

###
out.y <- list(name="y1", time=seq(0,24,by=2), lloq=2)
res2 <- simulx(project  = project1, 
               output   = out.y,
               settings = list(seed=12321) )
head(res2$y1, n=10)

###
out.y$limit <- 0
res3 <- simulx(project  = project1, 
               output   = out.y,
               settings = list(seed=12321) )
head(res3$y1, n=10)
 
     
###
project2 <- 'monolixRuns/warfarin_project.mlxtran'
y1 <- list(name="y1", lloq=8, limit=0)
y2 <- list(name="y2", uloq=40)

res4 <- simulx(project  = project2, 
               output   = list(y1, y2),
               settings = list(seed=123) )
head(res4$y1, n=11)
head(res4$y2, n=7)

library(gridExtra)
pl1 <- ggplotmlx() + 
  geom_point(data=res4$y1, aes(x=time, y=y1, colour=cens, group=id)) +
  theme(legend.position=c(.8, .8)) 
pl2 <- ggplotmlx() + 
  geom_point(data=res4$y2, aes(x=time, y=y2, colour=cens, group=id)) +
  theme(legend.position=c(.8, .2))
grid.arrange(pl1, pl2, ncol=2)


