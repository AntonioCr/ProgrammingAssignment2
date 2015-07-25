## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## solves for the matrix via passing values via nested functions and saves it to the global environment for future use
makeCacheMatrix <- function(x = matrix()) { # declares this a function expecting a matrix as a parameter
  invert <- NULL # creates empty variable named invert
  set <- function(y) { # creates function named set expects parameter of y
    x <<- y # assigns x to value y in global environment
    invert <<- NULL #creates empty variable named invert in global environment
  }
  get <- function() x # assigns get to retrieve x
  setinverse <- function(solve) invert <<- solve # assigns set inverse function to take solve as a parameter and assign it globally to invert 
  getinverse <- function() invert # assigns getinverse to retrieve invert 
  list(set = set, get = get, #sets expected list variables 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { # creates funciton, expects x and inherited parameters
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinverse() # assigns invert to named function of x  called getinverse 
  if(!is.null(invert)) { #tests to see if invert is not null 
    message("getting cached data") #message
    return(invert) #returns cached invert
  }
  data <- x$get() #assigns data to named function of x called get
  invert <- solve(data, ...) #assigns invert to solve data and any inherited parameters
  x$setinverse(invert) # calls setinverse named function of x with invert as a parameter 
  invert # evaluates this last for return
}
