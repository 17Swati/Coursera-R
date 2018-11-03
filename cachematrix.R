## To complete this assignment I am going to write a pair of 
## functions that cache the inverse of a matrix.

## Firstlty,I am going to write function - makeCacheMatrix: This
## function creates a special "matrix" object that can
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(Y){
    x <<- Y
    inv <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## After makeCacheMatrix I am going to write function for
## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cache result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}