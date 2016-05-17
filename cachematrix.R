## The following 2 functions creates a special matrix that caches its inverse
## and uses it in computations instead of recalculating each time the inverse
## of the matrix is required.  It the matrix changes, a new inverse is created
## and cached.

## This function creates the special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special matrix if it is NULL
##  or returns the inverse matrix x that was cached previously.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
