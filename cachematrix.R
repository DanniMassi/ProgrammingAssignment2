## This R script computes the inverse of a supplied matrix if no cached version of the inverse exists.
## If the inverse has already been cached then it is pulled from the cache and returned

## This function creates a special vector, which is a list of the following functions:
## set the value of the vector
## get the value of the vector
## set the value of the matrix inverse
## get the value of the matrix inverse
## returns this list of functions


makeCacheMatrix <- function(x = matrix()) {
  i <- matrix()
  set <- function(y) {
    x <<- y
    i <<- matrix() #accounts for when matrix changes
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the inverse is retrieved
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.na(i[1,1])) {
    message("Getting cached inverse")
    return(i)
  }
  inputMatrix <- x$get()
  i <- solve(inputMatrix)
  x$setinverse(i)
  
  #print("Original matrix:")
  #print(inputMatrix)
  #print("Inverse matrix:")
  return(i)
}