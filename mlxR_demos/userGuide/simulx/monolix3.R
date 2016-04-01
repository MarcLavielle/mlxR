setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)


project.file <- 'monolixRuns/theophylline_project.mlxtran'

#-------------------------------------
res1  <- simulx(project = project.file)

print(res1$population, digits=3)
print(ggplot(data=res1$y1) + 
        geom_point(aes(x=time, y=y1, colour=id)) + geom_line(aes(x=time, y=y1, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration") + theme(legend.position="none"))
#-------------------------------------
res2 <- simulx(project = project.file, 
               nrep    = 4,
               settings=list(seed=123456))
print(res2$population, digits=3)
head(res2$y1)
print(ggplotmlx(data=res2$y1,aes(x=time, y=y1, colour=id)) + 
        geom_point() +  geom_line() + facet_wrap(~ rep, ncol=2))

#-------------------------------------

res3a <- simulx(project = project.file, 
               npop    = 4,
               settings=list(seed=12345))
print(res3a$population, digits=3)
head(res3a$y1)
print(ggplotmlx(data=res3a$y1,aes(x=time, y=y1, colour=id)) + 
        geom_point() +  geom_line() + facet_wrap(~ pop, ncol=2))

res3b <- simulx(project = project.file, 
                npop    = 4,
                fim     = "lin",
                settings=list(seed=12345))
print(res3b$population, digits=3)

#-------------------------------------

pop4 <- simpopmlx(n=4, project=project.file)
print(pop4, digits=3)
res4 <- simulx(project   = project.file, 
               parameter = pop4)
print(res4$population, digits=3)

#-------------------------------------

res5a <- simulx(project = project.file, 
                npop    = 4,
                nrep    = 3,
                group   = list(size=20))
head(res5a$y1)

res5b <- simulx(project = project.file, 
                npop    = 4,
                nrep    = 3,
                group   = list(size=20),
                settings=list(disp.iter=TRUE))

g1 = list(size=100, treatment=list(amount=5, time=0))
g2 = list(size=100, treatment=list(amount=10, time=0))
res5c <- simulx(project = project.file, 
                npop    = 4,
                group   = list(g1, g2),
                result.file = "theo5c.csv")
head(read.table("theo5c.csv", header=T, sep=","))                

#-------------------------------------
