kmplotmlx  <-  function(r0)
{
  r0.name=attr(r0,"name")
  names(r0)[names(r0)==r0.name] <- "y"
  if (isfield(r0,"group")){
    g=as.numeric(levels(r0$group))[r0$group]
}else{
    g=rep(1,length(r0$id))
  }
  ng=max(g)
  S=NULL
  T=NULL
  G=NULL
  for (kg in seq(1,ng)){
    rk<-r0[g==kg,]
    t0=min(rk$time)
    ij<-which(rk$time>t0 & rk$y>0)
    sij=sort(rk$time[ij])
    nij=length(sij)
    N=length(unique(rk$id))
    Sk<-rep((N-seq(0,nij-1))/N,each=2)
    S<-c(S,Sk[1:(2*nij-1)])
    Tk<-rep(sij,each=2)
    T<-c(T,Tk[2:(2*nij)])
    G<-c(G,rep(kg,2*nij-1))
#    uij=uniquemlx(rk$id[ij])
  }
group=factor(G)
D=data.frame(T,S,group)
plot1=ggplot() +  geom_line(data=D, aes(x=T, y=S, colour=group)) + 
  xlab("time") + ylab("survival") 
print(plot1)
return(D)
}


#--------------------------------------------------------
uniquemlx <- function(x) 
{ 
  d <- !duplicated(x) 
  u=list(uniqueValue=x[d], firstIndex=which(d), sortIndex=match(x,x[d])) 
  return(u)
}
