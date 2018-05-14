## Since matrix inverse computation is generally expensive, it may be useful
## to store previous inverse calculations and reuse the value for future
## operations. This file defines a wrapper object for a matrix, which supports
## caching its inverse, as well as a function that computes the inverse in
## a way which automatically stores to or uses the cache.

## Creates a wrapper object for a matrix which also supports caching
## the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a matrix.
##
## If the inverse matrix has already been cached, the cached value is
## returned to save on computational effort. Else, the inverse is calculated
## and stored for reuse.
##
## Note that no checks are made to verify that a matrix is square or
## invertible, so errors may occur.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("using cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
