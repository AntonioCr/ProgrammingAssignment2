## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## solves for the matrix via passing values via nested functions and saves it to the global environment for future use
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invert <<- solve
  getinverse <- function() invert
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinverse(invert)
  invert
}
