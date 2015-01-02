#' support for matlab-like multi results
#' @export
list <- structure(NA,class="result")

#' @export
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  for(i in seq(along=args)) {
    a <- args[[i]]
    eval.parent(substitute(a <- v,list(a=a,v=value)))
  }
  x
}
