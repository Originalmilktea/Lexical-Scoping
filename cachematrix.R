## Put comments here that give an overall description of what your
## functions do

## Creates list object that can temporarily store inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## Finds inverse of matrix, or retrieves it if previously calculated in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  return(inv)
}
