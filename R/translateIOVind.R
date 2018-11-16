translateIOVind <- function(model, occ.name, nocc, o.iov) {
  
  sections <- sectionsModel(model)
  sm  <-  splitModel(model, sections)
  i.long <- which(sapply(sm, function(ch)  ch$name=="[LONGITUDINAL]"))
  
  lines <- c()
    sec.long <- splitSection(sm[[i.long]])
    long.lines <- iovinlongind(sec.long$input, o.iov, nocc, sec.long$name, occ.name)
    r1.long <- iovseclongind(sec.long, o.iov, nocc, occ.name)
    #    var.iov <- unique(c(var.iov, r0.long$iov))
    lines <- c(lines,"",long.lines,r1.long$lines)
  model <- "tempiov_model.txt"
  write(lines,model)
  return(list(model=model))
}

iovinlongind <- function(lines, o.iov, nocc, name, occ.name) {
  # adapts the input of section LONGITUDINAL for IOV 
  suffix <- "_iov"
  sep <- "([\\,\\{\\}])"
  #  vi <- c()
  
  for (expr in o.iov) {
    nexpr <- paste0(expr,suffix,1:nocc,collapse=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0))
      lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
    else
      lines <- c(lines, paste0("input={",nexpr,"}"))
    #    vi <- c(vi, expr)
  }
  lines <- c(lines, paste0("input={",occ.name,"}"), paste0(occ.name,"={use=regressor}"))
  return(lines=c(name,lines))
}



iovseclongind <- function(sec, o.iov=NULL, nocc, occ.name) {
  # add a block EQUATION to section LONGITUDINAL for IOV 
  
  suffix <- "_iov"
  new.lines <- c()
  if (!is.null(o.iov)) {
    new.lines <- c("EQUATION:",paste0("if ",occ.name,"==1"))
    for (ko in (1:nocc)) {
      for (vi in o.iov) {
        new.lines <- c(new.lines, paste0("   ",vi,"=",paste0(vi,suffix,ko)))
      }
      new.lines <- c(new.lines, paste0("elseif ",occ.name,"==",ko+1))
    }
    new.lines[length(new.lines)] <- "end"
  }
  for (k in (1:length(sec$blocks))) {
    new.lines <- c(new.lines,sec$blocks[k],sec$lines[[k]])
  }
  return(list(lines=new.lines))
}

