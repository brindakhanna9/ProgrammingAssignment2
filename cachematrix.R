
## This function essentially creates a matrix that sets the value
##and then gets the value of the matrix.

makeCacheMatrix <- function(x = matrix())
##creating a special matrix object that can cache its inverse
{
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    ##setting the value of the matrix
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  ##getting the value of the matrix
}



##This function essentially computes the inverse value of the matrix that is 
##given using the function above, and then returns that value.

cacheSolve <- function(x, ...) 
## Computing the inverse of the special "matrix" returned by makeCacheMatrix
## above.
{
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
    ## Returning a matrix that is the inverse of x
    
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}