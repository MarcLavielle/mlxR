processing_setting <- function(s1)
{
  s2=list()
  data.in=FALSE
  if (!is.null(s1)){
    if (isfield(s1,"record.file"))
    {s2$recordFile=s1$record.file}
    if (isfield(s1,"recordFile") )
    {s2$recordFile=s1$recordFile}
    if (isfield(s1,"loadDesign"))
    {s2$loadDesign=s1$load.design}
    if (isfield(s1,"load.design"))
    {s2$loadDesign=s1$load.design}
    if (isfield(s1,"seed"))
    {s2$seed=s1$seed
#     s2$loadDesign=TRUE
    }
    if (isfield(s1,"data.in"))
      data.in=s1$data.in
    
  }
  return(list(data.in,s2))
}



