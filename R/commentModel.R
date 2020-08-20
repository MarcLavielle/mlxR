commentModel  <-  function(model, parameters, test.project) {
  
  sections <- c("COVARIATE", "INDIVIDUAL","POPULATION")
  terms = splitModel( model,sections) 
  terms <- arrangeBlocs(terms)
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
  if (length(grep("EQUATION:",nmod))>0)
    nmod <- nmod[-grep("EQUATION:",nmod)]
  
  test.comment <- FALSE
  for (k in 1:length(parameters)) {
    nk <- names(parameters[[k]])
    ik <- which(nmod %in% nk)
    if (length(ik)>0) {
      lines[i1[ik]] <- paste0(";",lines[i1[ik]])
      lines[i1] <- gsub("correlation=",";correlation=",lines[i1])
      test.comment <- TRUE
    }
  }
  if (test.comment) {
    # if (varlevel)
    #   stop("individual parameter values cannot be used with IOV")
    long = splitModel( model,"LONGITUDINAL")[[1]]$lines
    if (!test.project) {
      model <- paste0(file_path_sans_ext(basename(model)),"_simulxModel.txt")
      test.project <- TRUE
    }
    write(c(long,lines),model)
  }
  
  return(list(model=model, test.project=test.project))
}

arrangeBlocs <- function(terms) {
  for (k in 1:length(terms)) {
    if (!is.null(terms[[k]]$lines)) {
      stk <- splitSection(terms[[k]]) 
      if (length(stk$blocks)>1) {
        lk <- c(paste0("[",stk$name,"]"), stk$input)
        j.eq <- grep("EQUATION:", stk$blocks)
        if (length(j.eq)>0) {
          lk <- c(lk, "EQUATION:")
        for (i in (1:length(j.eq)))
          lk <- c(lk, stk$lines[[j.eq[i]]])
        }
        j.def <- grep("DEFINITION:", stk$blocks)
        if (length(j.def)>0)  {
          lk <- c(lk, "DEFINITION:")
          for (i in (1:length(j.def)))
            lk <- c(lk, stk$lines[[j.def[i]]])
        }
        terms[[k]]$lines <- lk
      }
    }
  }
  return(terms)
}






