#------------------------------------------------------
model.pharmml <- 'pharmML/DelBene_2009_oncology_in_vitro_v1.xml'
#------------------------------------------------------

d = read.csv('data/delbene2009_data.csv',skip=1,na='.')
head(d)

N=length(unique(d$ID))
conc <- d$CONC[!duplicated(d$ID)]

p1 <- c(k1=0.0743, k2=0.0745, lambda0=0.0292, N0=2147.3, CV=0.1)
p2 <- list( name     = 'CONC',
            colNames = c('id', 'CONC'),
            value    = cbind(1:N, conc));

out  <- list( name = c('Nt','y'), time = unique(d$TIME[d$EVID!=1]))

res <- simulx( model     = model.pharmml,
               parameter = list(p1,p2),
               output    = out,
               settings  = list(seed=12345));

plot1 <- ggplotmlx() + 
  geom_line(data=res$Nt, aes(x=time, y=Nt, colour=id)) + 
  geom_point(data=res$y, aes(x=time, y=y,colour=id)) 
print(plot1)
