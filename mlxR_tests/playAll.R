playAll.dir=getwd()


ptr <- "\\.[R]$"
list.dir <- list.files(playAll.dir)  ##--- play all
list.dir <- list.dir[12:17]
for (nd in list.dir) {
  dos <- file.path(playAll.dir, nd)
  cat(dos,"\n")   
  for (nf in list.files(dos, pattern = ptr)) {
    cat(nf,"\n") 
    setwd(dos)
    source(file.path(dos,nf))
  }
}
