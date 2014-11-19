## This library provides a simple way to cache the inverse of a matrix to avoid recalculating it
## For example:
##    m <- makeCacheMatrix(matrix(rnorm(9), nrow=3, ncol=3))
##    s0 <- cacheSolve(m)
##    s1 <- cacheSolve(m)
##    s0 == s1
##    [,1] [,2] [,3]
##    [1,] TRUE TRUE TRUE
##    [2,] TRUE TRUE TRUE
##    [3,] TRUE TRUE TRUE


## Creates a wrapper around the provided matrix (or creates an empty matrix) 
## that allows for caching the inversion of the matrix
## Return value is a list with the following functions
##   get() : get the underlying matrix
##   set() : set the underlying matrix (clearing the cache)
##   setinv() : Set the inversion of this matrix (INTERNAL USE ONLY)
##   getinv() : Get the cached inversion of the matrix (INTERNAL USE ONLY)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## A wrapper around the solve function that caches the result for use in subsequent calls to 
## this function.  Only works on an object created with makeCacheMatrix.  
## Returns the inversion of the input.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
