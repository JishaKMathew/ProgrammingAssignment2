## Write an R function to cache potentially time-consuming computations.
## Assumption - The matrix supplied is always invertible


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## solve(X) returns the inverse of matrix X

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached inverse")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
        ## Return a matrix that is the inverse of 'x'
}
