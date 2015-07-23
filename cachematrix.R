## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function returns the inverse of matrix if in cache, otherwise calculates and returns
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(x, ...)
  x$setInverse(inv)
  inv
}
