isfield  <- function(structure, field)
{
  if (any(names(structure)==field)) TRUE else FALSE

} 
  
  # match("firstname", names(lis)).