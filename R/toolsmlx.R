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
  # [C,IA,IC]= unique( A , 'first' );
  C  <- unique( A )
  # IA <- match(data.frame(t(C)), data.frame(t(A)))
  # IC <- match(data.frame(t(A)), data.frame(t(C)))
  IA   = match( C, A )
  IC   = match( A , C )
  ans  =
    list(arg1=C, arg2=IA, arg3=IC)
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


dpopid <- function(x,s)
{
  n <- length(x)
  r <- list(name=s,j=NULL,id=NULL,N=NULL,pop=NULL,npop=NULL)
  for (k in (1:n))
  {
    xk <- x[[k]]
    if (is.list(xk)) {
      if (!is.data.frame(xk)){
        if (!is.null(xk$time) && is.data.frame(xk$time) )
          xk <- xk$time
        else
          xk <- NULL
      }
      if (!is.null(xk$id)){
        # idk <- as.factor(unique(xk$id))
        idk <- levels(factor(xk$id))
        # if (!is.null(r$id)) {
        # if (!identical(idk,r$id))
        #   stop(paste("Different id's are defined in ",s))
        # } else
        # r$id <- as.factor(idk)
        r$id <- unique(c(r$id,idk))
        # r$N <- unique(c(r$N,length(idk)))
        # if (length(r$N)>1)
        #   stop(paste('Different numbers of subjects are defined in ',s))
        r$j <- c(r$j,k)
      }
      if (!is.null(xk$pop)){
        r$npop <- unique(c(r$npop,length(unique(xk$pop))))
        if (length(r$npop)>1)
          stop(paste('Different numbers of populations are defined in ',s))
        r$pop <- c(r$pop,k)
      }
    } # else if (is.list(xk))
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
          if (is.null(select.id)) {
            select.id <- unique(datak$id)
          } else {
            select.id <- intersect(select.id , unique(datak$id))
          }
        } 
      }
    }
  }
  if (!is.null(select.id)) {
    if (length(select.id)==0)
      stop("Please check the id's... the intersection is empty")
    N <- nlevels(select.id)
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
  lines   <- readLines(con)
  close(con)
  
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


repCategories <- function(r, model) {
  categories <- NULL
  lines <- splitModel(model,"LONGITUDINAL")[[1]]$model
  if (!is.null(lines)) {
    ldef  <- grep("DEFINITION:",lines, fixed=TRUE)
    if (length(ldef)>0) {
      lines <- lines[-c(1:ldef)]
      lcat  <- grep("categories",lines, fixed=TRUE)
      if (length(lcat)>0) {
        r.names <- names(r)
        lines <- gsub(" ","",lines)
        lcat <- which(gregexpr("type=categorical",lines)>0)
        lci <- c(lcat-1, length(lines))
        for (j in (1:length(lcat))) {
          linej <- lines[lcat[j]]
          # jj <- regexpr("type=categorical",linej)
          # gsub("^.*?type=categorical","type=categorical",linej)
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


