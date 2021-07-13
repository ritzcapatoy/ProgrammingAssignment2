## This is a pair of functions that cache the mean of a matrix
## To use: Deliver the result of makeCacheMatrix call to cacheSolve

## @param x is an invertible matrix
## Example: 
## x = makeCacheMatrix(matrix(rnorm(9), 3, 3)), and
## x$set(matrix(rnorm(16), 4, 4))

makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Compute and cache the mean of a matrix
## @param x is the result of the previous makeCacheMatrix call
## @param ... additional arguments to pass to solve function
## Example:
## x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
## cacheSolve(x)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the mean of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
