## These 2 functions together are capable of storing the inverse of an inputted 
## matrix. The first one creates getter and setter functions for both the matrix
## and its inverse, and the 2nd function utilises them to initially calculate 
## and store them, then retrieve them on subsequent uses instead of 
## recalculating them.

## The makeCacheMatrix function creates an object which can store a matrix 
## vector and cache its inverse. It initially clears any previous calculated 
## values for the inverse of the inputted matrix, then returns a list of 
## functions to be utilised by cacheSolve to access the matrix and store its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <-function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## The cacheSolve function takes the object created by makeCacheMatrix and 
## checks to see if its inverse has already been calculated. If so, it retrieves 
## it. If not, it calculates it and stores it using objects created in 
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
  
}
