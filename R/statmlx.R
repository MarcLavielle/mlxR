statmlx <- function(r, stat, probs)
{
  list.stat <- c("mean","sd","median","var") 
  l.stat <- length(stat)
  n.stat <- NULL
  for (j in (1:l.stat)){
    if (stat[j] %in% list.stat)
      n.stat <- c(n.stat,stat[j])
    else 
      n.stat <- c(n.stat,paste0("p",probs*100))
  }
  
  r <- r[!is.na(names(r))]
  rs <- list()  
  for (j in (1:length(r))){
    rj <- r[[j]]  
    rj <- rj[,!colnames(rj) == "id"]
    srj <- NULL
    if (names(r)[j]=="parameter"){
      if (is.null(rj$group)){
        n.srj <- NULL
        for (k in (1:l.stat)){
          stat.k <- stat[k]       
          if (stat.k %in% list.stat){
            srj <- rbind(srj,sapply(rj,stat.k))
            n.srj <- rbind(n.srj,paste0(names(rj),".",stat.k))
          } else {
            srj <- rbind(srj,sapply(rj,stat.k,probs=probs))
            aa <- apply(expand.grid(names(rj),paste0("p",probs*100)), 1, paste, collapse=".")  
            n.srj <- rbind(n.srj,t(matrix(aa,nrow=ncol(rj))))
          }
        }
        srj <- as.vector(srj)
        names(srj) <- n.srj
        srj <- as.data.frame(t(srj))
      } else {
        ng <- length(unique(rj$group))
        n.srj <- NULL
        for (ig in (1:ng)) {
          srjg <- NULL
          rjg <- rj[rj$group==ig,-1]
          for (k in (1:l.stat)){
            stat.k <- stat[k]       
            if (stat.k %in% list.stat){
              srjg <- rbind(srjg,sapply(rjg,stat.k))
              if (ig==1)
                n.srj <- rbind(n.srj,paste0(names(rjg),".",stat.k))
            } else {
              srjg <- rbind(srjg,sapply(rjg,stat.k,probs=probs))
              if (ig==1){
                aa <- apply(expand.grid(names(rjg),paste0("p",probs*100)), 1, paste, collapse=".")  
                n.srj <- rbind(n.srj,t(matrix(aa,nrow=ncol(rjg))))
              }
            }
          }
          srj <- rbind(srj, c(ig,as.vector(srjg)))
        }
        srj <- as.data.frame(srj)
        names(srj) <- c("group",as.vector(n.srj))
      }
      
    } else {
      
      ny0 <- names(r)[j]
      iy <- which(names(rj) == ny0)
      names(rj)[iy] <- "y"
      for (k in (1:l.stat)){
        stat.k <- stat[k]
        if (is.null(rj$group)){
          if (stat.k %in% list.stat)
            srjk <- aggregate(y~time,data=rj,stat.k)
          else 
            srjk <- aggregate(y~time,data=rj,stat.k,probs=probs)
          if (k==1)
            srj <- as.data.frame(cbind(srjk[[1]],srjk[[2]]))
          else
            srj <- cbind(srj,srjk[[2]])
        } else {
          if (stat.k %in% list.stat)
            srjk <- aggregate(y~group+time,data=rj,stat.k)
          else 
            srjk <- aggregate(y~group+time,data=rj,stat.k,probs=probs)      
          if (k==1)
            srj <- as.data.frame(cbind(srjk[[1]],srjk[[2]],srjk[[3]]))
          else
            srj <- cbind(srj,srjk[[3]])
        }
      }
      
      srj <- as.data.frame(srj)
      if (is.null(rj$group))
        names(srj) <- c("time",paste0(ny0,".",n.stat))
      else {
        names(srj) <- c("group","time",paste0(ny0,".",n.stat))
        srj <- srj[order(srj$group),]
      }
    }
    rownames(srj) <- NULL
    rs[[j]] <- srj
  }
  return(rs)
}



