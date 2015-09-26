## A matrix abstraction that can cache its inverse. Can be used to speed up
## potentially expensive calls to solve() on the same matrix.
##
## Usage:
##  m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
##  c <- makeCacheMatrix(m)
##  cacheSolve(c) # Calls solve(m)
##  cacheSolve(c) # Returns cached

## Create an environment containing the original matrix x, and its inverse.
## Provide accessor functions for set and get both the matrix, and the cached
## inverse, which is initially set to NULL. Reset cache if the underlying
## matrix is changed.
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(i) {
    cached_inverse <<- i
  }
  getinverse <- function() {
    cached_inverse
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute the inverse of the cached matrix x. Returns the cached value if it
## has already been computed for the underlying matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
}
