commentModel  <-  function(model, parameters, test.project) {
  
  sections <- c("COVARIATE", "INDIVIDUAL","POPULATION")
  terms = splitModel( model,sections) 
  nmod <- NULL
  lines <- NULL
  i1 <- NULL
  for (i in 1 : length( terms)) {
    ni <- length(lines)
    li <- terms[[i]]$lines
    lines <- c(lines, li)
    jdi <- grep("DEFINITION:",li, fixed=TRUE)
    if (length(jdi)>0) {
      gi <- grep("\\=.*",li[(jdi+1):length(li)])
      i1 <- c(i1, ni+jdi+gi)
      nmod <- c(nmod, gsub("\\=.*","",li[(jdi+1):length(li)]))
    }
  }
  test.comment <- FALSE
  for (k in 1:length(parameters)) {
    nk <- names(parameters[[k]])
    ik <- which(nmod %in% nk)
    if (length(ik)>0) {
      lines[i1[ik]] <- paste0(";",lines[i1[ik]])
      test.comment <- TRUE
    }
  }
  if (test.comment) {
    # if (varlevel)
    #   stop("individual parameter values cannot be used with IOV")
    long = splitModel( model,"LONGITUDINAL")[[1]]$lines
    if (!test.project) {
      model <- paste0(model,"_simulxModel.txt")
      test.project <- TRUE
    }
    write(c(long,lines),model)
  }
  
  return(list(model=model, test.project=test.project))
}

