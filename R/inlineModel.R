#' inline model
#' 
#' Define a model "inline"
#' 
#' A temporary model file \code{filename} is created.  Default name is "temp_model.txt". 
#' \code{filename="random"} generates a random name.
#'  
#' @param str model
#' @param filename name of the temporary model file 
#' 
#' @return A Shiny app with files ui.R, server.R and model.txt
#' @examples
#' \dontrun{
#' myModel1 <- inlineModel("
#' [LONGITUDINAL]
#' EQUATION:
#' f = 10*exp(-0.2*t)
#' ")
#' 
#' print(myModel1)
#' 
#' r <- simulx(model=myModel1, output=list(name="f", time=0:100))
#' 
#' myModel2 <- inlineModel("
#' [LONGITUDINAL]
#' EQUATION:
#' f = 10*exp(-0.2*t)
#' ", filename="random")
#' 
#' print(myModel2)
#' }
#' @export
inlineModel <- function(str, filename=NULL) {
  if (is.null(filename)) {
    r <- list(filename="tempModel.txt", str=str)
  } else if (identical(filename, "random")) {
    uuiidd <- uuid()
    filename <-paste0("tempModel_",uuiidd,".txt")
    r <- list(filename=filename, str=str)
  } else {
    write(str, filename)
    r <- filename
  }
  return(r)
}
