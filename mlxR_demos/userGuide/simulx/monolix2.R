setwd(dirname(parent.frame(2)$ofile))
# library(mlxR)

project.file <- 'monolixRuns/theophylline_project.mlxtran'

#------------------------------------------------
N <- 30
res1  <- simulx(project = project.file,
                group   = list(size = N))

print(ggplotmlx(data=res1$y1) + 
        geom_point(aes(x=time, y=y1, colour=id)) +
        geom_line(aes(x=time, y=y1, colour=id)) +
        scale_x_continuous("Time") + scale_y_continuous("Concentration") +
        theme(legend.position="none"))

print(res1$treatment)

print(res1$originalId)

res2  <- simulx(project = project.file,
                group   = list(size = N),
                settings  = list(replacement=T))
print(res2$originalId)

#------------------------------------------------
N <- 5
res3  <- simulx(project = project.file,
                parameter = "mode",
                group   = list(size = N))
print(res3$originalId)

#------------------------------------------------
N 		<- 100
weight <- data.frame(id = (1:N), WEIGHT = rnorm(n=N, mean=70, sd=10))
adm   <- list(time = 1, amount = 500)
out2  <- list(name = 'y1', time = seq(0, 25, by=2))
outw  <- c("WEIGHT","V")

res4 <- simulx(project = project.file, 
               output = list(outw, out2),
               treatment = adm,
               parameter = weight)

print(head(res4$parameter))

print(ggplotmlx(data=res4$y1,aes(x=time, y=y1, by=id)) +  geom_point() +
        scale_x_continuous("Time") + scale_y_continuous("Concentration"))








