## The following functions are used to create a special object that stores a matrix and caches its inverse.

## The function "makeCacheMatrix" creates a special "matrix", which allows us to set the value of the matrix, get the value of the matrix, set the value of its inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
