translateIOV <- function(model, occ.name, nocc, output, iov0, cat0=NULL) {
  
  if (length(cat0)==0)  cat0 <- NULL
  sections <- sectionsModel(model)
  sm  <-  splitModel(model, sections)
  lines <- c()
  v.iov <- var.iov <- iov0
  
  cat0.name <-  unlist(lapply(cat0, function(x) x$name))
  if ("OCC" %in% cat0.name)
    rem.name <- c("occ","socc", "sOCC")
  else
    rem.name <- c("occ","socc", "OCC","sOCC")
  if (!is.null(cat0))
    rem.name <- unique(c(rem.name,paste0("s",cat0.name)))
  
  i.cov <- which(sapply(sm, function(ch)  ch$name=="[COVARIATE]"))
  if (length(i.cov)>0) {
    sec.cov <- splitSection(sm[[i.cov]])
    #    rem.name <- c(occ.name)
    #    rem.name <- c(occ.name, unlist(lapply(cat0, function(x) x$name)))
    r0.cov <- iovin(sec.cov$input, v.iov, nocc, sec.cov$name, rem.name=rem.name)
    v.iov <- r0.cov$iov
    lines.cov <- r0.cov$lines
    var.iov <- v.iov
    if (length(sec.cov$blocks)) {
      for (k in (1:length(sec.cov$blocks))) {
        if (identical(sec.cov$blocks[k],'DEFINITION:'))
          rk.cov <- iovdef(sec.cov$lines[[k]], v.iov, nocc)
        else
          rk.cov <- ioveq(sec.cov$lines[[k]], v.iov, nocc)
        v.iov <- rk.cov$iov
        var.iov <- unique(c(var.iov, v.iov))
        lines.cov <- c(lines.cov, rk.cov$lines)
      }
    }
    lines <- c(lines,lines.cov)
  }
  
  i.ind <- which(sapply(sm, function(ch)  ch$name=="[INDIVIDUAL]"))
  if (length(i.ind)>0) {
    sec.ind <- splitSection(sm[[i.ind]])
    r0.ind <- iovin(sec.ind$input, v.iov, nocc, sec.ind$name, cat0, rem.name=rem.name)
    v.iov <- r0.ind$iov
    lines.ind <- r0.ind$lines
    for (k in (1:length(sec.ind$blocks))) {
      if (identical(sec.ind$blocks[k],'DEFINITION:'))
        rk.ind <- iovdef(sec.ind$lines[[k]], v.iov, nocc)
      else
        rk.ind <- ioveq(sec.ind$lines[[k]], v.iov, nocc)
      v.iov <- rk.ind$iov
      var.iov <- unique(c(var.iov, v.iov))
      lines.ind <- c(lines.ind, rk.ind$lines)
    }
    lines <- c(lines,"",lines.ind)
  }
  
  i.long <- which(sapply(sm, function(ch)  ch$name=="[LONGITUDINAL]"))
  if (length(i.long)>0) {
    sec.long <- splitSection(sm[[i.long]])
    v.iov <- addiov(var.iov, v.iov, output)
    r0.long <- iovinlong(sec.long$input, v.iov, nocc, sec.long$name, occ.name)
    r1.long <- iovseclong(sec.long, r0.long$iov, nocc, occ.name)
    #    var.iov <- unique(c(var.iov, r0.long$iov))
    lines <- c(lines,"",r0.long$lines,r1.long$lines)
  }
  model <- "tempiov_model.txt"
  write(lines,model)
  #file.edit(model)
  return(list(model=model, iov=v.iov, occ.name=occ.name, cat=cat0))
}

regstr1 <-function(expr,str) {
  # find in a set of equations (str) which variables are function of variable expr
  strb <- paste0(" ",str," ")
  #sep <- "([+-\\*\\^\\(\\)\\/[:space:]])"
  sep <- "([^[:digit:]_])"
  ig  <- grep(paste0(sep,expr,sep),strb)
  newv <- gsub("\\=.*","",str[ig])
  return(newv)
}

repstr1 <-function(expr,x,str) {  
  # add x to ALL the occurences of expr (and only expr) in str
  str <- paste0(" ",str," ")
  #sep <- "([+-\\*\\^\\(\\)\\/[:space:]^[:digit:]_])"
  sep <- "([^[:digit:]_])"
  str <- gsub(paste0(sep,expr,sep),paste0("\\1",expr,x,"\\2"),str)
  str <- gsub("^\\s+|\\s+$", "", str)
  return(str)
}


line2field <- function(str) {
  if (length(str)>1)
    str <- strmerge(str)
  str <- gsub(" ","",str)
  rout <- list()
  for (k in (1:length(str))) {
    strk <- str[k]
    r <- list(name=sub("\\=.*","",strk))
    i1 <- regexpr("\\{",strk)
    i2 <- tail(gregexpr("\\}",strk)[[1]],n=1)
    strk <- substr(strk,i1+1,i2-1)
    sp <- strsplit(strk,",")[[1]]
    sp <- strmerge(sp)
    fields <- sub("\\=.*","",sp)
    rv <- sub(".*\\=","",sp)
    nv <- lapply(rv, function(x) sum(gregexpr("\\{",x)[[1]]>0))
    lrv <- as.list(rv)
    iv1 <- which(nv==1)
    if (length(iv1)>0) {
      riv1 <- gsub(".*?\\{(.*?)\\}.*", "\\1", rv[iv1])
      lrv[iv1] <- strsplit(riv1,",")
    }
    iv2 <- which(nv>=2)
    if (length(iv2)>0) {
      for (i2 in (iv2)) {
        rvi2 <- rv[i2]
        i1 <- regexpr("\\{",rvi2)
        i2 <- tail(gregexpr("\\}",rvi2)[[1]],n=1)
        rvi2 <- substr(rvi2,i1+1,i2-1)
        j1 <- -1
        j2 <- -1
        ni2 <- nchar(rvi2)
        lrvi2 <- c()
        while (j2>=j1 && j2 <ni2) {
          j1 <- j2+2
          if (substr(rvi2,j1,j1)=="{") 
            j2 <- regexpr("\\}",substr(rvi2,j1+1,ni2))+j1
          else 
            j2 <- regexpr("\\,",substr(rvi2,j1+1,ni2))+j1-1
          if (j2>=j1)
            lrvi2 <- c(lrvi2, substr(rvi2,j1,j2))
        }
        lrv[[iv2]] <- lrvi2
      }
    }
    
    r$fields <- lrv
    names(r$fields) <- fields
    rout[[k]] <- r
  }
  if (length(rout)==1)  rout <- rout[[1]]
  return(rout)
}

field2line <- function(r) {
  #write line(s) with fields
  nr <- length(r)
  if (nr==1) r <- list(r)
  lines <- vector(length=nr)
  for (k in (1:nr)) {
    rk <- r[[k]]
    nk <- names(rk$fields)
    mk <- sapply(rk$fields,length)
    ik <- which(mk>1)
    rk$fields[ik] <- sapply(rk$fields[ik], function(x) paste0("{",paste(x,collapse=","),"}"))
    uk <- paste(nk,unlist(rk$fields),sep="=") 
    lines[k] <- paste0(rk$name,"={",paste(uk, collapse=","),"}")
  }
  return(lines)
}

strmerge <- function(str1) {
  n <- length(str1)
  str2 <- c()
  idx <- 0
  while (idx<n) {
    idx <- idx + 1
    s <- str1[idx]
    n1 <- sum(gregexpr("\\{",s)[[1]]>0)
    n2 <- sum(gregexpr("\\}",s)[[1]]>0)
    while(n1 > n2) {
      idx <- idx+1
      if (identical(substr(s,nchar(s),nchar(s)),","))
        s <- paste0(s,str1[idx])
      else
        s <- paste(s,str1[idx],sep=",")
      n1 <- sum(gregexpr("\\{",s)[[1]]>0)
      n2 <- sum(gregexpr("\\}",s)[[1]]>0)
    }
    str2 <- c(str2, s)
  }
  return(str2)
}

splitSection  <-  function(section) {
  #   Split a section of a model into multiple blocks (input/equation/definition) 
  
  lines <- section$lines
  i.input   = grep("input=",lines, fixed=TRUE)[1]
  i.eq   = grep("EQUATION:",lines, fixed=TRUE)
  i.def   = grep("DEFINITION:",lines, fixed=TRUE)
  i.pk   = grep("PK:",lines, fixed=TRUE)
  is <- sort(c(i.pk,i.eq,i.def))
  is <- c(is, length(lines)+1)
  bt <- c()
  bc <- list()
  if (length(is)>1) {
    for (k in (1:(length(is)-1))) {
      if (regexpr("EQUATION:",lines[is[k]], fixed=TRUE)==1)
        bt <- c(bt, "EQUATION:")
      else if (regexpr("DEFINITION:",lines[is[k]], fixed=TRUE)==1)
        bt <- c(bt, "DEFINITION:")
      else
        bt <- c(bt, "PK:")
      if (is[k+1]-is[k] >1)
        bc[[k]] <- lines[(is[k]+1):(is[k+1]-1)]
      else
        bc[[k]] <- ""
    }
  }
  return(list(name=section$name, input=lines[i.input:(is[1]-1)], blocks=bt, lines=bc))
}

iovin <- function(lines, v.iov, nocc, name, cat=NULL, rem.name=NULL) {
  # duplicates the list of variables with IOV in the input list
  
  if (!is.null(rem.name)) {
    vc <- sub("\\=.*","",lines)
    lines <- lines[!(vc %in% rem.name)]
    lines <- gsub(paste(paste0(",",rem.name,","),collapse="|"),",",lines)
    lines <- gsub(paste(paste0("\\{",rem.name,","),collapse="|"),"\\{",lines)
    lines <- gsub(paste(paste0(",",rem.name,"\\}"),collapse="|"),"\\}",lines)
    lines <- gsub(paste(paste0("=",rem.name),collapse="|"),"=",lines)
    lines <- lines[lines!="input="]
    gl <- grep("\\{\\}",lines)
    if (length(gl)>0)
      lines <- lines[-gl]
  }
  
  if (length(lines)==0)
    return(list(iov=NULL, lines=name))
  
  suffix <- "_iov"
  sep <- "([\\,\\{\\}])"
  vi <- c()
  v.iov <- setdiff(v.iov,rem.name)
  
  for (expr in v.iov) {
    nexpr <- paste0(expr,suffix,1:nocc,collapse=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0))
      vi <- c(vi, expr)
    lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
    vc <- sub("\\=.*","",lines)
  }
  if (!is.null(v.iov))
    lines <- lines[!(vc %in% v.iov)]
  if (!is.null(cat)) {
    for (k in (1: length(cat))) {
      lines <- c(lines, paste0("input = {", paste0(cat[[k]]$name,suffix,1:nocc,collapse=","),"}"))
      for (ko in (1:nocc)) {
        lo <- paste0(cat[[k]]$name,suffix,ko,"={type=categorical,categories={",paste(cat[[k]]$categories,collapse = ","),"}}")
        lines <- c(lines, lo)
      }
    }
  }
  return(list(iov=vi, lines=c(name,lines)))
}


iovinlong <- function(lines, v.iov, nocc, name, occ.name) {
  # adapts the input of section LONGITUDINAL for IOV 
  suffix <- "_iov"
  sep <- "([\\,\\{\\}])"
  #  vi <- c()
  
  for (expr in v.iov) {
    nexpr <- paste0(expr,suffix,1:nocc,collapse=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0))
      lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
    else
      lines <- c(lines, paste0("input={",nexpr,"}"))
    #    vi <- c(vi, expr)
  }
  lines <- c(lines, paste0("input={",occ.name,"}"), paste0(occ.name,"={use=regressor}"))
  return(list(iov=v.iov, lines=c(name,lines)))
}


ioveq <- function(lines, v.iov, nocc) {
  # duplicates lines in a set of equation creating new variables for each occasion
  
  suffix <- "_iov"
  listc <- c()
  for (kc in (1: length(v.iov))) {
    listk <- testk <- v.iov[kc]
    while (length(testk)>0) {
      listk <- unique(c(listk,regstr1(testk[1],lines)))
      testk <- unique(c(testk,regstr1(testk[1],lines)))
      testk <- testk[-1] 
    }
    listc <- unique(c(listc, listk))
  }
  new.eq <- c()
  for (ko in (1:nocc)) {
    smek <- lines
    for (exprk in listc) {
      smek <- repstr1(exprk,paste0(suffix,ko),smek)
    }
    new.eq <- c(new.eq, smek)
  }
  new.eq <- unique(new.eq)
  return(list(iov=listc,lines=c("EQUATION:",new.eq)))
}

iovseclong <- function(sec, v.iov, nocc, occ.name) {
  # add a block EQUATION to section LONGITUDINAL for IOV 
  
  suffix <- "_iov"
  new.lines <- c()
  if (!is.null(v.iov)) {
    new.lines <- c("EQUATION:",paste0("if ",occ.name,"==1"))
    for (ko in (1:nocc)) {
      for (vi in v.iov) {
        new.lines <- c(new.lines, paste0("  ",vi,"=",paste0(vi,suffix,ko)))
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

iovdef <- function(lines, v.iov, nocc) {
  # define IOV by creating two blocks DEFINITION
  
  suffix <- "_iov"
  lines <- gsub(",mean=",",reference=",lines)
  lines <- gsub(",prediction=",",reference=",lines)
  lines <- gsub(",typical=",",reference=",lines)
  iop.sd <- (length(grep("sd=",lines))>0) 
  fields <- line2field(lines)
  i.iov <- which(sapply(fields, function(x) !is.null(x$fields$varlevel)))
  vcv <- list()
  for (k in (1:length(fields))) {
    icv <- match(fields[[k]]$fields$covariate,v.iov)
    icv <- icv[!is.na(icv)]
    vcv[[k]] <- v.iov[icv]
    if (length(icv)>0) 
      i.iov <- unique(c(i.iov, k))
  }
  if (length(i.iov)>0) {
    v.iov <- sapply(fields[i.iov], function(x) x$name)
    f1 <- fields
    for (iv in i.iov) {
      f1[[iv]]$name <- paste0(f1[[iv]]$name,"0")
      f1[[iv]]$fields$varlevel <- NULL
      f1[[iv]]$fields$covariate <- NULL
      f1[[iv]]$fields$coefficient <- NULL
      if (iop.sd)
        f1[[iv]]$fields$sd <- f1[[iv]]$fields$sd[1]
      else
        f1[[iv]]$fields$var <- f1[[iv]]$fields$var[1]
    }
    line1 <- field2line(f1)
    
    f2 <- fields
    for (iv in i.iov) {
      f2[[iv]]$fields$varlevel <- NULL
      f2[[iv]]$fields$reference <- f1[[iv]]$name
      if (iop.sd)
        f2[[iv]]$fields$sd <- ifelse(is.na(f2[[iv]]$fields$sd[2]),0,f2[[iv]]$fields$sd[2])
      else
        f2[[iv]]$fields$var <- ifelse(is.na(f2[[iv]]$fields$var[2]),0,f2[[iv]]$fields$var[2])
    }
    f2o <- f2[i.iov]
    f2 <- list()
    for (ko in (1:nocc)) {
      for (k in 1:length(i.iov))  {
        f2o[[k]]$name <- paste0(v.iov[k],suffix,ko)
        if (length(vcv)>0) {
          vcvk <- vcv[[i.iov[k]]]
          if (length(vcvk)>0) {
            fck <- fields[[i.iov[k]]]$fields$covariate
            for (j in (1:length(vcvk)))
              fck <- gsub(vcvk[j],paste0(vcvk[j],suffix,ko),fck)
            f2o[[k]]$fields$covariate <- fck
          }
        }
      }
      f2 <- c(f2,f2o)
      line2 <- field2line(f2)
    }
    new.lines <- c("DEFINITION:",line1,"DEFINITION:",line2)
  } else {
    new.lines <- c("DEFINITION:", lines)
  }
  return(list(iov=v.iov,lines=new.lines))
}

addiov <- function(var.iov, v.iov, output) {
  # add the outputs with IOV in the list of variables with IOV in section LONGITUDINAL
  
  for (k in 1:length(output)) {
    nk <- output[[k]]$name
    ik <- match(nk, var.iov)
    nk <- nk[!is.na(ik)]
    ik <- ik[!is.na(ik)]
    if (length(ik)>0) {
      v.iov <- unique(c(v.iov, nk))
    }
  }
  return(v.iov)
}

outiov <- function(output,v.iov,occ, v.iov0) {
  new.output <- list()
  j <- 0
  for (k in 1:length(output)) {
    nk <- output[[k]]$name
    ik <- match(nk, v.iov)
    nk <- nk[!is.na(ik)]
    ik <- ik[!is.na(ik)]
    if (length(ik)>0) {
      output[[k]]$name <- setdiff(output[[k]]$name, nk)
      if (is.data.frame(occ[[1]])) 
        outk <- list(name=nk, time=occ[[1]])
      else
        outk <- list(name=nk, time=occ[[1]]$time)
      
      j <-j+1
      new.output[[j]] <- outk
      nk <- setdiff(nk, v.iov0)
      if (length(nk)>0) {
        j <-j+1
        new.output[[j]] <- list(name=paste0(nk,"0"))
      }
    }
  }
  output <- c(output, new.output)
}

param.iov <- function(p, occ) {
  
  suffix <- "_iov"
  occ <- occ[[1]]
  p2 <- p
  if (is.data.frame(occ)) {
    no <- names(occ)
    occ.name <- setdiff(no,c("id","time"))
    nocc <- length(unique(occ[[occ.name]]))
  } else {
    occ.name <- occ$name
    nocc <- length(occ$time)
    no <- c("id", "time", occ.name)
  }
  v.iov <-  c()
  cat <- list()
  indj <- 0
  for (k in (1:length(p))) {
    pk <- p[[k]]
    if (is.data.frame(pk) && (!is.null(pk$time))) {
      jfk <- which(sapply(pk,is.character)|sapply(pk,is.factor))
      jfk <- jfk[names(jfk)!="id"]
      if (length(jfk)>0) {
        #        pk[,jfk] <- as.factor(pk[,jfk])  #OJO
        for (jf in (1:length(jfk))) {
          indj <- indj+1
          cat[[indj]] <- list(name=names(pk)[jfk[jf]], categories=levels(pk[,jfk[jf]]))
        }
      }
      nk <- names(pk)
      so <- setdiff(nk, no)
      if (length(so)==0)
        so <- occ.name
      n.param <- length(so)
      if (!("id" %in% nk)) 
        pk$id <- 1
      
      N <- length(unique(pk$id))
      io <- intersect(nk, no)
      if ( (is.data.frame(occ) && (!identical(pk[io],occ[io]))) | (!is.data.frame(occ) && any(pk$time!=rep(occ$time,N))) )  
        stop("\n occasions defined in the varlevel field and the parameters are different\n")
      if (is.data.frame(occ))
        mo <- merge(occ, pk)
      else 
        mo <- merge(data.frame(occ=1:nocc,time=occ$time),pk)
      
      dk <- vector(length=n.param)
      for (j in (1:n.param)) {
        pkj <- mo[c("id",so[j])]
        dk[j] <- (dim(unique(pkj))[1] == N) 
      }
      pk[so[!dk]] <- NULL
      pk[c("time",occ.name)] <- NULL
      if (!("id" %in% nk)) 
        pk$id <- NULL
      if (dim(pk)[2]>1)
        p2[[k]] <- unique(pk)
      else
        p2[[k]] <- NULL
      
      pk.occ <- mo
      pk.occ[so[dk]] <- NULL 
      pk.occ["time"] <- NULL
      if (!identical(occ.name,"occ")) {
        pk.occ$occ <- pk.occ[occ.name]
        pk.occ[occ.name] <- NULL
      }
      pkn <- subset(pk.occ, occ==1)
      io <-  which(names(pkn) %in% so[!dk])
      names(pkn)[io] <- paste0(so[!dk],suffix,1)
      pkn$occ <- NULL
      for (ko in (2:nocc)) {
        pko <- subset(pk.occ, occ==ko)
        names(pko)[io] <- paste0(so[!dk],suffix,ko)
        pko$occ <- NULL
        pkn <- merge(pkn,pko, by=c("id"), all=TRUE)
      }
      for (kf in (1:dim(pkn)[2])) {
        if (is.factor(pkn[,kf]))
          pkn[is.na(pkn[,kf]),kf] <- levels(pkn[,kf])[1]
        else
          pkn[is.na(pkn[,kf]),kf] <- 0
      }
      if (!("id" %in% nk)) {
        pkn$id <- NULL
        pkn <- as.vector(pkn)
      }
      p2[[length(p2)+1]] <- pkn
      v.iov <- c(v.iov, so[!dk])
    }
  }
  p2 <- p2[sapply(p2,length)>0]
  return(list(param=p2, iov=v.iov, cat=cat))
}

