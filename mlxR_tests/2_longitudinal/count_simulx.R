setwd(dirname(parent.frame(2)$ofile))

p <- list(name=c('a','b'), value=c(10,0.5))
l <- list(name='lambda', time=seq(0, 100, by=1))
y <- list(name='y', time=seq(0, 100, by=4))
out <- list(l, y);
res <- simulx(model='count_model.txt', parameter=p, output=out)

print(ggplotmlx(aes(x=time, y=lambda), data=res$lambda) + geom_line(size=1) +
  geom_point(aes(x=time, y=y), data=res$y, color="red") + ylab("") )