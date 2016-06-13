patchCor <- function(model)
{
  
  lines <- readLines(model)
  cortxt <-  grep('correlation', lines, fixed=TRUE, value=TRUE)
  if(length(cortxt))
    lines <- gsub("level=id,","",lines)
  write(lines,model)
}