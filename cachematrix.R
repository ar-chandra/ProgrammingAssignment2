## The following pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initializing the matrix inverse
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y){
     x <<- y
     inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(matrixinverse) inv <<- matrixinverse
    
  # get the inverse of the matrix
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  # Calculate the inverse using solve function of R
  
  invmatrix <- solve(data,...)
  
  x$setinverse(invmatrix)
  
 # Return a matrix that is the inverse of 'x'
  invmatrix
}
