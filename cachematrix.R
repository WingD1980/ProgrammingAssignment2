## The following two functions are designed and work together to cache the inverse of a matrix to avoid computing the matrix inversion repeatly

## The first function creates a matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function computes the inverse from the inverse of the matrix returned by the first function. If the inverse has already been calculated
## and the matrix has not changed, then this function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
  if(!is.null(m)) {
    message("getting inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
