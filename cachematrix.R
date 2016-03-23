## the following function creates
## a special matrix 
## called inverse matrix 
## that caches its inverse
## using as Script Model from the example "Caching the Mean of a Vector"

makeCacheMatrix <- function(mtx = matrix()) {
  inversemat <- NULL
  set <- function(x) {
    mtx <<- x;
    inversemat <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inversemat <<- inv;
  getinv <- function() return(inversemat);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## The function solve computes the inverse of one matrix 
## The matrix have to be an invertible matrix
## The cacheSolve functtion will retrieve it from the cache

cacheSolve <- function(mtx, ...) {
  inversemat <- mtx$getinv()
  if(!is.null(inversemat)) {
    message("Getting cached data...")
    return(inversemat)
  }
  data <- mtx$get()
  inversemat <- solve(data, ...)
  mtx$setinv(inversemat)
  return(inversemat)
}

## try this test
X <- matrix(rpois(36,6), nrow = 6)
dX <- makeCacheMatrix(X)
dX$get()
cacheSolve(dX)
