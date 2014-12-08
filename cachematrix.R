## Put comments here that give an overall description of what your
## functions do
## This set of functions creates a special matrix that can cache its inverse, 
##  then computes the inverse of the special matrix initially returned. 
##  The second function also checks to see if the inverse is alreaday calculated and
##  cached, and if it is it retrieves the inverse from cache rather than recalculating it.
## Write a short comment describing this function

## This first function creates the special matrix that will cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## The second function calculates the inverse of our special matrix created by the first function.
## But first it checks to see if the mean has already been calculated and, if so,
## retrieves the inverse from cache and skips the computation.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m       ## Return a matrix that is the inverse of 'x'
}
