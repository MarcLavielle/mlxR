# returns the indices corresponding to the positions of s in the array x
findstrcmp <- function(x, s , not=FALSE ){
  if(not){
    which( x != s ) 
  } else {
    which( x == s )
  }
  
}

funique <- function(A){
  #   [C,IA,IC] = unique(A) also returns index vectors IA and IC such that
  #   C = A(IA) and A = C(IC).  
  #
  # [C,IA,IC]= unique( A , 'first' )
  C  <- unique( A )
  # IA <- match(data.frame(t(C)), data.frame(t(A)))
  # IC <- match(data.frame(t(A)), data.frame(t(C)))
  IA   = match( C, A )
  IC   = match( A , C )
  ans  = list(arg1=C, arg2=IA, arg3=IC)
}

uniquemlx <- function(x) { 
  d <- !duplicated(x) 
  u=list(uniqueValue=x[d], firstIndex=which(d), sortIndex=match(x,x[d])) 
  return(u)
}



fsort <- function(X){
  #   [Y,I] = sort(X,DIM,MODE) also returns an index matrix I.
  #   If X is a vector, then Y = X(I).    
  
  Y    = sort( X )
  I    = match( Y, X )
  ans  = list(arg1=Y, arg2=I)
}

mklist <- function(x, add.name=T)
{
  s <- list()
  if (is.list(x) && is.null(names(x)))
  {
    n <- length(x)
    for (k in (1:n))
    {
      xk <- x[[k]]
      if (is.list(xk) && is.null(names(xk)))
      {
        nk <- length(xk)
        for (j in (1:nk))
        {
          xkj <- xk[[j]]
          if (!is.null(xkj))
          {
            if (is.list(xk) && is.null(names(xk)))
              s[[length(s)+1]]=xkj
            else
              s[[length(s)+1]]=list(xkj)
          }
        }
      } 
      else 
      {
        if (!is.null(xk))
        {
          if (is.character(xk))
          {
            if (add.name==T)  
              s[[length(s)+1]]=list(name=xk) 
            else
              s[[length(s)+1]]=xk
          }
          else
            s[length(s)+1]=list(xk) 
        }
      }
    }
  } 
  else 
  {
    if (!is.null(x))
      s[length(s)+1]=list(x)
    else
      s <- NULL
  }
  for (k in (1:length(s)))
  {
    sk <- s[[k]]
    if (is.list(sk))
    {
      if (!is.null(sk$header))
      {
        sk$colNames <- sk$header
        sk$header <- NULL
      }
      if (!is.null(sk$colNames))
      {
        dsk <- data.frame(sk$value)
        names(dsk) <- sk$colNames
        if (!is.null(sk$name) && "time" %in% sk$colNames)
          s[[k]] <- list(name=sk$name, time=dsk)
        else
          s[[k]] <- dsk
      } 
      else if (!is.null(sk$value) && is.null(sk$time))
      {
        s[[k]] <- sk$value
        names(s[[k]]) <- sk$name
      }
    }
  }
  return(s)
}


dpopid <- function(x,s) {
  n <- length(x)
  r <- list(name=s,j=NULL,id=NULL,N=NULL,pop=NULL,npop=NULL)
  for (k in (1:n)) {
    xk <- x[[k]]
    if (is.list(xk)) {
      if (!is.data.frame(xk)){
        if (!is.null(xk$time) && is.data.frame(xk$time) )
          xk <- xk$time
        else
          xk <- NULL
      }
      if (is.null(xk$id) & !is.null(xk$ID) ) {
        warning("\n'ID' is used instead of 'id' in an input argument of 'simulx'\n")
        xk$id <- xk$ID
        xk$ID <- NULL
      }
      
      if (!is.null(xk$id)) {
        xk$id <- factor(xk$id, levels=unique(xk$id))
        idk <- levels(factor(xk$id))
        r$id <- unique(c(r$id,idk))
        r$j <- c(r$j,k)
      }
      if (!is.null(xk$pop)){
        r$npop <- unique(c(r$npop,length(unique(xk$pop))))
        if (length(r$npop)>1)
          stop(paste('Different numbers of populations are defined in ',s), call.=FALSE)
        r$pop <- c(r$pop,k)
      }
    } 
  }
  if (!is.null(r$id))
    r$N <- length(r$id)
  return(r)
}  


resample.data  <- function(data,idOri,N=NULL,new.id=NULL,replacement=F)
{
  n <- length(unique(idOri))
  if (is.null(new.id)) {
    if (replacement==TRUE){
      new.id <- sample(1:n,N,replace=TRUE)
    } else{
      K <- floor(N/n)
      new.id <- c(rep(1:n,K),sort(sample(1:n,N-K*n,replace=FALSE)))
    }
  } else {
    N <- length(new.id)
  }
  
  new.idOri=idOri[new.id]
  data$N <- NULL
  data$idOri <- NULL  
  data$id <- NULL
  uN <- as.factor(1:N)
  for  (j in (1:length(data)))
  {
    dataj <- data[[j]]
    for  (k in (1:length(dataj)))
    {
      datak <- dataj[[k]]
      
      if (is.data.frame(datak))
      {
        if (!is.null(datak$id))
        {
          ik  <- which(names(datak)=="id")
          idk <- datak$id
          dkv=NULL
          id1 <- as.character(idk)
          #id1 <- as.numeric(as.character(idk))
          for (i in 1:N)
          {
            id2 <- as.character(idOri[new.id][i])
            #id2 <- as.numeric(as.character(idOri[new.id][i]))
            ji <- which(id1==id2)
            if (length(ji)>0)
            {
              dkji <- datak[ji,]
              dkji[,ik] <- i
              dkv <- rbind(dkv,dkji,deparse.level=0)
            }
          } 
          dvkid <- as.factor(c(dkv$id, (1:N)))
          dkv$id <- dvkid[1:length(dkv$id)]
          data[[j]][[k]] <- dkv
        }
      }
      else if (is.list(datak))
      {
        for (m in (1:length(datak)))
        {
          datam <- datak[[m]]
          if (is.data.frame(datam) && !is.null(datam$id))
          {
            ik  <- which(names(datam)=="id")
            idk <- datam$id
            id1 <- as.character(idk)
            #id1 <- as.numeric(as.character(idk))
            dkv=NULL
            for (i in 1:N)
            {
              id2 <- as.character(idOri[new.id][i])
              #id2 <- as.numeric(as.character(idOri[new.id][i]))
              ji <- which(id1==id2)
              if (length(ji)>0) {
                dkji <- datam[ji,]
                dkji[,ik] <- i
                dkv <- rbind(dkv,dkji,deparse.level=0)
              }
            } 
            dvkid <- as.factor(c(dkv$id, (1:N)))
            dkv$id <- dvkid[1:length(dkv$id)]
            data[[j]][[k]][[m]] <- dkv
          }
        }
      }
    }
  }
  data$id <- data.frame(newId=as.factor(seq(1:N)),oriId=idOri[new.id])
  return(data)
}

select.data  <- function(data)
{
  select.id <- NULL
  for  (j in (1:length(data))) {
    dataj <- data[[j]]
    for  (k in (1:length(dataj))) {
      datak <- dataj[[k]]
      if (is.data.frame(datak)) {
        if (!is.null(datak$id)) {
          if (is.null(select.id)) {
            select.id <- unique(datak$id)
          } else {
            select.id <- union(select.id , unique(datak$id))
          }
        } 
      } else {
        for  (m in (1:length(datak))) {
          datam <- datak[[m]]
          if (is.data.frame(datam)) {
            if (!is.null(datam$id)) {
              if (is.null(select.id)) {
                select.id <- unique(datam$id)
              } else {
                select.id <- union(select.id , unique(datam$id))
              }
            } 
          }
        }
      }
    }
  }
  if (!is.null(select.id)) {
    if (length(select.id)==0)
      stop("\nPlease check the id's... The selection of the id's for the different inputs of simulx is not consistent: the intersection is empty!\n", call.=FALSE)
    N <- nlevels(select.id)
    for  (j in (1:length(data))) {
      dataj <- data[[j]]
      for  (k in (1:length(dataj))) {
        datak <- dataj[[k]]
        if (is.data.frame(datak)) {
          if (!is.null(datak$id)) {
            idk <- which(datak$id %in% select.id)
            data[[j]][[k]] <- datak[idk,]
          } 
        }
      }
    }
    data$id <- data.frame(newId=as.factor(seq(1:length(select.id))),oriId=select.id)
  }
  return(data)
}

unlistRec <- function(x,s=NULL)
{
  if (is.list(x) && is.null(names(x))){
    n <- length(x)
    for (k in (1:n)){
      r <- unlist(x[[k]],s)
    }
  } else {
    s[[length(s)+1]]=x
    z <- list(x,s)
    return(z)
  }
}

modify.mlxtran <- function(model, addlines)
{
  con     <- file(model, open = "r")
  lines   <- readLines(con, warn=FALSE)
  close(con)
  lines <- gsub("\\;.*","",lines)
  
  if (!is.list(addlines[[1]]))
    addlines <- list(addlines)
  
  for (k in (1:length(addlines))) {
    section <- addlines[[k]]$section
    if (is.null(section))
      section <- "[LONGITUDINAL]"
    block <- addlines[[k]]$block 
    if (is.null(block))
      block <- "EQUATION:"
    idx1 <- grep(section, lines, fixed=TRUE)
    new.lines <- lines[1:idx1]
    lines <- c(lines[(idx1+1):length(lines)],"")
    
    idx2 <- grep("[",lines, fixed=TRUE)
    if (length(idx2)==0) {
      idx2 <- length(lines)
    } else {
      idx2 <- idx2[1]
    }
    lines <- c(new.lines,lines[1:(idx2-1)], block, addlines[[k]]$formula, "", lines[idx2:length(lines)])
  }
  model <- "temp_model.txt"
  write(lines,model)
  return(model)
}

rct.mlxtran <- function(model, addlines)
{
  con     <- file(model, open = "r")
  lines   <- readLines(con, warn=FALSE)
  close(con)
  lines <- gsub("\\;.*","",lines)
  test.w <- FALSE
  i1 <- grep("event", lines)
  if (length(i1)>0) {
    test.w <- TRUE
    lines[i1] <- gsub(" ","",lines[i1])
    for (k in (1:length(i1))) {
      ik1 <- i1[k]
      lk1 <- lines[ik1]
      if (length(grep("type=event", lk1))>0) {
        ik2 <- grep("}",lines[ik1:length(lines)])[1] + ik1 -1
        lk <- lines[ik1:ik2]
        if (length(grep("rightCensoringTime", lk))==0) {
          if (length(grep("maxEventNumber", lk))==0) 
            stop("Right censoring time should be defined for repeated events, when there is no maximum number of events", call.=FALSE)
          lines[ik1] <- gsub("type=event","type=event, rightCensoringTime=1e10",lk[1])
        }
        if (length(grep("intervalCensored", lk))>0 & length(grep("intervalLength", lk))==0) 
          stop("Interval length should be defined when events are interval censored", call.=FALSE)
      }
    }
  }
  i1 <- grep("correlation", lines)
  if (length(i1)>0) {
    test.w <- TRUE
    lines[i1] <- gsub(" ","",lines[i1])
    lines[i1] <- gsub("level=id,","",lines[i1])
  }
  if (test.w) {
    model <- "temp_model.txt"
    write(lines,model)
  }
  return(model)
}

repCategories <- function(r, model) {
  categories <- NULL
  lines <- splitModel(model,"LONGITUDINAL")[[1]]$lines
  if (!is.null(lines)) {
    ldef  <- grep("DEFINITION:",lines, fixed=TRUE)
    if (length(ldef)>0) {
      lines <- lines[-c(1:ldef[1])]
      lcat  <- grep("categories",lines, fixed=TRUE)
      if (length(lcat)>0) {
        r.names <- names(r)
        lines <- gsub(" ","",lines)
        lcat <- which(gregexpr("type=categorical",lines)>0)
        lci <- c(lcat-1, length(lines))
        for (j in (1:length(lcat))) {
          linej <- lines[lcat[j]]
          wj <- which(strsplit(linej,"type=categorical")[[1]][1] == paste0(r.names,"={"))
          if (length(wj)>0) {
            linesj <- lines[c((lci[j]+1):(lci[j+1]))]
            linesj <- linesj[which(gregexpr("categories=",linesj)>0)]
            str <- gsub("^.*?categories","categories",linesj)
            str <- sub(".*?\\{(.*?)\\}.*", "\\1", str)
            categories <- strsplit(str,",")[[1]]
            rwj <- as.factor(r[[wj]][[r.names[wj]]])
            levels(rwj) <- categories
            r[[wj]][[r.names[wj]]] <- rwj
          }
        }
      }
    }
  }
  return(r)
}


