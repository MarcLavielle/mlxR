generateModelFromPkModel <-  function(parameter,output)
{
  p_name   = parameter$name
  p_length = length(p_name)
  str1     = p_name[[1]]
  for(k in seq(2,p_length)){
    str1 = paste(str1,p_name[[k]],sep=", ")
  }
  model_txt="
          [LONGITUDINAL]
          input = {param.list}
          EQUATION:
          output = pkmodel(param.list)
          "
  model_txt = gsub("param.list",str1,model_txt)
  if(length(output$name)==1){
    str2 = output$name[[1]]
  }else{
    str2 = paste("{",output$name[[1]],",",output$name[[2]],"}")
  }    
  model_txt = gsub("output",str2,model_txt)
  model     = inlineModel(model_txt)
  
  return(model)
}