findstrcmp <- function(x, s , not=FALSE ) 
{
#   ans = STRCMP(x,s) compares  x and s and returns the indices corresponding to 
#     the positions of s in the array x
  ans = NULL
  n = length(x)
  for (i in 1 :n)
  {
    if(x[i]==s)
    {
      ans = c(ans, i)
    }
  }
  if (not == TRUE)
    ans       = setdiff(c(1:n), ans) #  vérifier
  
  ans
}

funique <- function(A)
{
  #   [C,IA,IC] = unique(A) also returns index vectors IA and IC such that
  #   C = A(IA) and A = C(IC).  
  #
  # [C,IA,IC]= unique( A , 'first' );
  C    = unique( A )
  IA   = match( C, A )
  IC   = match( A , C )
  ans  =
    list(arg1=C, arg2=IA, arg3=IC)
}

fsort <- function(X)
{
  #   [Y,I] = sort(X,DIM,MODE) also returns an index matrix I.
  #   If X is a vector, then Y = X(I).    
  
  Y    = sort( X )
  I    = match( Y, X )
  ans  = list(arg1=Y, arg2=I)
}
