translateIOV <- function(model, occ.name, nocc, output, iov0, cat0=NULL) {
  
  if (length(cat0)==0)  
    cat0 <- NULL
  sections <- sectionsModel(model)
  sm  <-  splitModel(model, sections)
  lines <- c()
  i.iov <- iov0
  o.iov <- addiov(i.iov, output)
  
  cat0.name <-  unlist(lapply(cat0, function(x) x$name))
  # if ("OCC" %in% cat0.name)
  #   rem.name <- c("occ","socc", "sOCC")
  # else
  rem.name <- c("occ","socc", "OCC","sOCC")
  if (!is.null(cat0))
    rem.name <- unique(c(rem.name,paste0("s",cat0.name)))
  
  i.cov <- which(sapply(sm, function(ch)  ch$name=="[COVARIATE]"))
  d.iov <- c.iov <- v.iov <- NULL
  if (length(i.cov)>0) {
    sec.cov <- splitSection(sm[[i.cov]])
    #    rem.name <- c(occ.name)
    #    rem.name <- c(occ.name, unlist(lapply(cat0, function(x) x$name)))
    r0.cov <- iovin(sec.cov$input, NULL, i.iov, nocc, sec.cov$name, cat0, rem.name=rem.name)
    i.iov <- r0.cov$iov
    lines.cov <- r0.cov$lines
    #var.iov <- v.iov
    if (length(sec.cov$blocks)) {
      for (k in (1:length(sec.cov$blocks))) {
        if (identical(sec.cov$blocks[k],'EQUATION:')) {
          rk.cov <- ioveq(sec.cov$lines[[k]], i.iov, d.iov, nocc)
        } else {
          rk.cov <- iovdef(sec.cov$lines[[k]], i.iov, nocc)
          d.iov <- rk.cov$d.iov
        }
        c.iov <- rk.cov$iov
        #        var.iov <- unique(c(var.iov, v.iov))
        lines.cov <- c(lines.cov, rk.cov$lines)
      }
    }
    lines <- c(lines,lines.cov)
    o.iov <- unique(c(o.iov,addiov(c.iov, output)))
  }
  
  i.ind <- which(sapply(sm, function(ch)  ch$name=="[INDIVIDUAL]"))
  if (length(i.ind)>0) {
    sec.ind <- splitSection(sm[[i.ind]])
    #    u.iov <- setdiff(unique(c(c.iov, o.iov)), i.iov)
    u.iov <- c.iov
    u.iov <- setdiff(c.iov, cat0.name)
    r0.ind <- iovin(sec.ind$input, u.iov, i.iov, nocc, sec.ind$name, cat0, rem.name=rem.name)
    #    v.iov <- r0.ind$iov
    if (!is.null(r0.ind$iov)) {
      v.iov <- unique(c(u.iov, i.iov))
      o.iov <- unique(c(o.iov,addiov(c.iov, output)))
      lines.ind <- r0.ind$lines
      for (k in (1:length(sec.ind$blocks))) {
        if (identical(sec.ind$blocks[k],'EQUATION:')) {
          rk.ind <- ioveq(sec.ind$lines[[k]], v.iov, d.iov, nocc)
          v.iov <- rk.ind$iov
        } else {
          rk.ind <- iovdef(sec.ind$lines[[k]], v.iov, nocc)
          d.iov <- rk.ind$d.iov
          v.iov <- rk.ind$iov
        }
        #var.iov <- unique(c(var.iov, v.iov))
        lines.ind <- c(lines.ind, rk.ind$lines)
        lines <- c(lines,"",lines.ind)
      }
    } else {
      lines <- c(lines,"",sm[[i.ind]]$lines)
    }
  }
  
  i.long <- which(sapply(sm, function(ch)  ch$name=="[LONGITUDINAL]"))
  if (length(i.long)>0) {
    if (!is.null(v.iov) & !is.null(r0.ind$iov)) {
      sec.long <- splitSection(sm[[i.long]])
      u.iov <- unique(c(i.iov,o.iov))
      long.lines <- iovinlong(sec.long$input, v.iov, u.iov, nocc, sec.long$name, occ.name)
      r1.long <- iovseclong(sec.long, v.iov, d.iov, u.iov, nocc, occ.name)
      #    var.iov <- unique(c(var.iov, r0.long$iov))
      lines <- c(lines,"",long.lines,r1.long$lines)
    } else {
      lines <- c(lines,"",sm[[i.long]]$lines)
    }
  }
  model <- "tempiov_model.txt"
  write(lines,model)
  return(list(model=model, iov=unique(c(o.iov,v.iov,i.iov)), occ.name=occ.name, cat=cat0))
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
    str <- strmerge(str, 0)
  str <- gsub(" ","",str)
  rout <- list()
  for (k in (1:length(str))) {
    strk <- str[k]
    r <- list(name=sub("\\=.*","",strk))
    i1 <- regexpr("\\{",strk)
    i2 <- tail(gregexpr("\\}",strk)[[1]],n=1)
    strk <- substr(strk,i1+1,i2-1)
    sp <- strsplit(strk,",")[[1]]
    sp <- strmerge(sp, 1)
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

strmerge <- function(str1, op=0) {
  n <- length(str1)
  str2 <- c()
  idx <- 0
  while (idx<n) {
    idx <- idx + 1
    si <- str1[idx]
    n1 <- sum(gregexpr("\\{",si)[[1]]>0)
    n2 <- sum(gregexpr("\\}",si)[[1]]>0)
    while(n1 > n2) {
      idx <- idx+1
      if (identical(substr(si,nchar(si),nchar(si)),",") | identical(substr(si,nchar(si),nchar(si)),"{"))
        si <- paste0(si,str1[idx])
      else
        si <- paste(si,str1[idx],sep=",")
      n1 <- sum(gregexpr("\\{",si)[[1]]>0)
      n2 <- sum(gregexpr("\\}",si)[[1]]>0)
    }
    str2 <- c(str2, si)
  }
  
  if (op==1) {
    n <- length(str2)
    str3 <- c()
    idx <- 0
    while (idx<n) {
      idx <- idx + 1
      si <- str2[idx]
      if (!identical(si, "no-variability")) {
        while (!grepl("=",si)) {
          idx <- idx + 1
          si <- paste(si, str2[idx], sep=",")
        }
      }
      str3 <- c(str3, si)
    }
    str3 <- gsub(",}","}",str3)
    return(str3) 
  } else {
    str2 <- gsub(",}","}",str2)
    return(str2)
  }
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

iovin <- function(lines, c.iov=NULL, v.iov=NULL, nocc, name, cat=NULL, rem.name=NULL) {
  # duplicates the list of variables with IOV in the input list
  
  if (!is.null(rem.name)) {
    vc <- sub("\\=.*","",lines)
    lines <- lines[!(vc %in% rem.name)]
    foo <- "foo123456"
    lines <- gsub(paste0(rem.name,collapse="|"),foo,lines)
    lines <- gsub(paste0(",",foo),"",lines)
    lines <- gsub(paste0(foo,","),",",lines)
    lines <- gsub("\\{,","\\{",lines)
    lines <- gsub(",\\}","\\}",lines)
    lines <- gsub(paste(paste0("=",rem.name),collapse="|"),"=",lines)
    lines <- lines[lines!="input="]
    lines <- lines[lines!=paste0("input=",foo)]
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
  
  for (expr in c.iov) {
    nexpr0 <- paste0(expr,"0")
    nexpr1 <- paste0("eta_",expr,suffix,1:nocc,collapse=",")
    nexpr <- paste(nexpr0, nexpr1, sep=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0)) {
      vi <- c(vi, expr)
      lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
    } else {
      lines <- c(lines, paste0("input={",nexpr,"}"))
    }
  }
  
  for (expr in v.iov) {
    nexpr <- paste0(expr,suffix,1:nocc,collapse=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0))
      vi <- c(vi, expr)
    lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
  }
  vc <- sub("\\=.*","",lines)
  
  if (!is.null(c(c.iov,v.iov)))
    lines <- lines[!(vc %in% c(c.iov,v.iov))]
  if (!is.null(cat)) {
    for (k in (1: length(cat))) {
      if (!(cat[[k]]$name %in% rem.name)) {
        if (!(cat[[k]]$name %in% vi))
          lines <- c(lines, paste0("input = {", paste0(cat[[k]]$name,suffix,1:nocc,collapse=","),"}"))
        for (ko in (1:nocc)) {
          lo <- paste0(cat[[k]]$name,suffix,ko,"={type=categorical,categories={",paste(cat[[k]]$categories,collapse = ","),"}}")
          lines <- c(lines, lo)
        }
      }
    }
  }
  return(list(iov=vi, lines=c(name,lines)))
}


iovinlong <- function(lines, v.iov, o.iov, nocc, name, occ.name) {
  # adapts the input of section LONGITUDINAL for IOV 
  suffix <- "_iov"
  sep <- "([\\,\\{\\}])"
  #  vi <- c()
  
  for (expr in v.iov) {
    nexpr0 <- paste0(expr,"0")
    nexpr1 <- paste0("eta_",expr,suffix,1:nocc,collapse=",")
    nexpr <- paste(nexpr0, nexpr1, sep=",")
    if (any(regexpr(paste0(sep,expr,sep),lines)>0))
      lines <- gsub(paste0(sep,expr,sep),paste0("\\1",nexpr,"\\2"),lines)
    else
      lines <- c(lines, paste0("input={",nexpr,"}"))
    #    vi <- c(vi, expr)
  }
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


ioveq <- function(lines, v.iov=NULL, d.iov=NULL, nocc) {
  # duplicates lines in a set of equation creating new variables for each occasion
  
  new.lines <- c()
  if (length(d.iov)>0) {
    for (i in 1:length(v.iov)) {
      vi <- v.iov[i]
      di <- d.iov[i]
      vi0 <- paste0(vi,"0")
      vis <- paste0(vi,"_iov")
      vie <- paste0("eta_",vi,"_iov")
      for (k in (1:nocc)) {
        if (tolower(di)=="normal") {
          nl <- paste0(vis,k," = ",vi0," + ",vie,k)
        } else if (tolower(di)=="lognormal") {
          nl <- paste0(vis,k," = ",vi0," * exp(",vie,k,")") 
        } else if (tolower(di)=="logitnormal") {
          nl <- paste0(vis,k," = 1/(1+exp(-logit(",vi0,") + ",vie,k,"))")
        } else {
          stop("IOV is only possible with distributions normal, lognormal, logitnormal", call.=FALSE)
        }
        new.lines <- c(new.lines, nl)
      }
    }
  }
  
  
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
      smek <- repstr1(exprk,paste0("_iov",ko),smek)
    }
    new.eq <- c(new.eq, smek)
  }
  new.eq <- unique(new.eq)
  return(list(iov=listc,lines=c("EQUATION:",new.lines,new.eq)))
}

iovseclong <- function(sec, v.iov=NULL, d.iov=NULL, o.iov=NULL, nocc, occ.name) {
  # add a block EQUATION to section LONGITUDINAL for IOV 
  
  suffix <- "_iov"
  new.lines <- c()
  if (!is.null(v.iov)) {
    new.lines <- c("EQUATION:",paste0("if ",occ.name,"==1"))
    for (ko in (1:nocc)) {
      for (vi in v.iov) {
        new.lines <- c(new.lines, paste0("   eta_",vi,"=",paste0("eta_",vi,suffix,ko)))
      }
      for (vi in o.iov) {
        new.lines <- c(new.lines, paste0("   ",vi,"=",paste0(vi,suffix,ko)))
      }
      new.lines <- c(new.lines, paste0("elseif ",occ.name,"==",ko+1))
    }
    new.lines[length(new.lines)] <- "end"
  }
  for (i in 1:length(v.iov)) {
    vi <- v.iov[i]
    di <- d.iov[i]
    if (tolower(di)=="normal") {
      nl <- paste0(vi," = ",vi,"0 + eta_",vi)
    } else if (tolower(di)=="lognormal") {
      nl <- paste0(vi," = ",vi,"0 * exp(eta_",vi,")") 
    } else if (tolower(di)=="logitnormal") {
      nl <- paste0(vi," = 1/(1+exp(-logit(",vi,"0) + eta_",vi,"))")
    } else {
      stop("IOV is only possible with distributions normal, lognormal, logitnormal", call.=FALSE)
    }
    new.lines <- c(new.lines, nl)
  }
  for (k in (1:length(sec$blocks))) {
    new.lines <- c(new.lines,sec$blocks[k],sec$lines[[k]])
  }
  return(list(lines=new.lines))
}

iovdef <- function(lines, v.iov=NULL, nocc) {
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
    d.iov <- sapply(fields[i.iov], function(x) x$fields$distribution)
    for (iv in i.iov) {
      if (identical(fields[[iv]]$fields$varlevel,"id*occ")) {
        if (iop.sd)
          fields[[iv]]$fields$sd <- c(0,fields[[iv]]$fields$sd)
        else
          fields[[iv]]$fields$var <- c(0,fields[[iv]]$fields$var)
      }
    }
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
      f2[[iv]]$fields$reference <- 0
      f2[[iv]]$fields$distribution <- "normal"
      if (iop.sd)
        f2[[iv]]$fields$sd <- ifelse(is.na(f2[[iv]]$fields$sd[2]),0,f2[[iv]]$fields$sd[2])
      else
        f2[[iv]]$fields$var <- ifelse(is.na(f2[[iv]]$fields$var[2]),0,f2[[iv]]$fields$var[2])
    }
    f2o <- f2[i.iov]
    f2 <- list()
    for (ko in (1:nocc)) {
      for (k in 1:length(i.iov))  {
        f2o[[k]]$name <- paste0("eta_",v.iov[k],suffix,ko)
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
    }
    line2 <- field2line(f2)
    new.lines <- c("DEFINITION:",line1,line2)
  } else {
    new.lines <- c("DEFINITION:", lines)
    d.iov <- NULL
  }
  new.lines <- gsub("no-variability=no-variability","no-variability",new.lines)
  return(list(iov=v.iov,d.iov=d.iov,lines=new.lines))
}

addiov0 <- function(var.iov, v.iov, output) {
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

addiov <- function(v.iov, output) {
  # add the outputs with IOV in the list of variables with IOV in section LONGITUDINAL
  o.iov <- NULL
  for (k in 1:length(output)) {
    nk <- output[[k]]$name
    ik <- match(nk, v.iov)
    nk <- nk[!is.na(ik)]
    ik <- ik[!is.na(ik)]
    if (length(ik)>0) {
      o.iov <- unique(c(o.iov, nk))
    }
  }
  return(o.iov)
}

outiov <- function(output,v.iov,occ, v.iov0) {
  new.output <- list()
  j <- 0
  for (k in 1:length(output)) {
    nk <- output[[k]]$name
    ik <- match(nk, c(v.iov,v.iov0))
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
      # if ( (is.data.frame(occ) && (!identical(pk[io],occ[io]))) | 
      #      (!is.data.frame(occ) && any(pk$time!=rep(occ$time,N))) )  
      #   stop("\n occasions defined in the varlevel field and the parameters are different\n", call.=FALSE)
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
          pkn[is.na(pkn[,kf]),kf] <- NaN
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

