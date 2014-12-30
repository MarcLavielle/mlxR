pharmml2mlxtran  <- function(model.input, model.output=NULL)
{
  if (is.null(model.output))
     model.output = paste0(file_path_sans_ext(model.input),"_tr.txt")
  
  session<-Sys.getenv("session.simulx")
  zz=file.path(session,'lib','lixoftLanguageTranslator')
  str=paste0('"',zz,'" --from=pharmml --to=mlxtran --input-file=',model.input)  
  str=paste0(str,' --output-file=',model.output,' --option=with-observation-model') 
  system(str, wait=T)
  
  return(model.output)
}

