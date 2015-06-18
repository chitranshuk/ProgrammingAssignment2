## This programming project is mainly for constructing a R function that is able to cache potentially 
## time-consuming computations.If the computation are time consuming and the same computation is repeated
## several times in the programe then it makes sense to cahce the value and use it whenever its needed instead
## of recomputing it everytime

## The project has two functions 1) makeCacheMatrix and 2) cacheSolve. The first one will create the special matrix and
## the second one will return the inverse. It will either calculate the inverse if it is not done earlier or fetch
## it from the cache if it was done earlier

## makeCacheMatrix: This is a function that creates a special "matrix" object that can cache its inverse.
## This function does the following    
    ## sets the value of the matrix
    ## gets the value of the matrix
    ## sets  the inverse of the matrix
    ## gets the inverse of the matrix
    
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
