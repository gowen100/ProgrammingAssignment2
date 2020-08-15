## Put comments here that give an overall description of what your	
## functions do	

## This function creates a special "matrix" object that can cache its inverse.
## Takes in a matrix object (defaulted to an empty matrix) and outputs four functions
## all defined in the local environment
## set,get - set and get the matrix object
## setinv,getinv - set and get the inverse matrix object

makeCacheMatrix <- function(x = matrix()) {	
  m <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## his function computes the inverse of the special "matrix" object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {	
  ## Return a matrix that is the inverse of 'x'	
  m <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}