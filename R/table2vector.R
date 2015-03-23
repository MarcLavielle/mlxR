#' Converts a table into a vector 
#' @param f: the table 
#' @return  v: the vector
#'  @export
#' 
table2vector <- function(f) 
{
  t <- read.table(f,header=TRUE)
  v <- t$value
  names(v) <- t$name
  return(v)
}
