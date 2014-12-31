#' inline model
#' 
#' @param str model
#' @param filename where to write the temporary model
#' @export
inlineModel <- function(str,filename="temp_model.txt"){
  write(str,filename)
  return(filename)
}
