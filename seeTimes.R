seeTimes <- function (n = numeric)
{
  for (i in 1:2)
  {
    ## Shows how to create the special "matrix" object 
    ## calling 'makeCacheMatrix'.
    if (i==1) 
    {
      ## Generate a square matrix of dimension n,
      ## using random elements.
      my_matrix <- matrix(rnorm(n*n), n, n)
      
      ## Create a special "matrix" object that can
      ## cache its inverse.
      my_special_matrix <- makeCacheMatrix(my_matrix)
    }
    
    ## Shows how to replace the original matrix into an
    ## already created special "matrix" object invoking
    ## the 'set' method.
    if (i==2) 
    {
      ## (Re)generate the elements of the original
      ## matrix.
      my_matrix <- matrix(rnorm(n*n), n, n)
      
      ## Replace the original matrix into an already
      ## created special "matrix" object.
      my_special_matrix$set(my_matrix)
    }
    
    ## First  call to cacheSolve().
    ## Print the time required to compute the inverse
    ## and cache the result.
    print(system.time(cacheSolve(my_special_matrix)))
    
    ## Second call to cacheSolve().
    ## Print the time required to retrieve the already
    ## cached result.
    print(system.time(cacheSolve(my_special_matrix)))
  }
  
  ## Return some funny message
  return ("Do you see the difference? :)")	 
}

