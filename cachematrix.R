## This is a pair of functions that caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setsolve <- function(solve) {m <<- solve}
  getsolve <- function() {m}
  list( set = set, get = get,
        getsolve = getsolve,
        setsolve = setsolve)
}


## This function returns a matrix that is the inverse of 'x'.  If it had already
## been previously computed it will take the cached inverse of 'x' rather than
## recomputing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
