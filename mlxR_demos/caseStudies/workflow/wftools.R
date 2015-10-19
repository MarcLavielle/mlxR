#-------------------------------------
addgroup <- function(r0, rv, str)
{
  ic <- which(names(rv)==str)
  rs <- rv[order(rv[,3],rv[,1]),]
  ur <- uniquemlx(rs[,3])
  ui <- uniquemlx(rs[,1])
  ir <- data.frame(id=ui$value, group=as.factor(ur$sortIndex[ui$firstIndex]))
  ir <- ir[order(ir$id),]
  r1 <- merge(r0,ir,by="id")
  attr(r1,"name")=attr(r0,"name")
  return(r1)
}

uniquemlx <- function(x) 
{ 
  d <- !duplicated(x) 
  u=list(value=x[d], firstIndex=which(d), sortIndex=match(x,x[d])) 
  return(u)
}
