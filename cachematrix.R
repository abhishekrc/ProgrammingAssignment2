## R Programming - Assignment 2

## These functions can be used to find the inverse of a matrix.
## If the inverse has been calculated previously, the function returns 
## the cached value (instead of rerunning the inversion process).

## Create a "matrix" object that can cache its inverse for later use
makeCacheMatrix <- function(x = matrix()) {
  # Set "inverse" object to null
  i <- NULL
  # Function to set "inverse" and "input" objects
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Function to get the user-input matrix
  get <- function() {
    x
  }
  # Function which sets the "inverse" object to the calculated value
  # Used when cache is NOT available
  setInverse <- function(inv) {
    i <<- inv
  }
  # Function to get the "inverse" object from cache
  # Used when cache is available
  getInverse <- function() {
    i
  }
  # Return the list of above functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Compute the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cached value is displayed
cacheSolve <- function(x, ...) {
  # Set the value of the "inverse" object from cache
  i <- x$getInverse()
  data <- x$get()
  # Check for two conditions
  # (1) If the cached value exists 
  # (2) If the input matrix has not changed
  if(!is.null(i) && identical(data,x) ) {
    #If cache exists, return the cached "inverse" object
    message("Getting value from cache")
    return(i)
  }
  # Either the cache does not exist or input has changed,
  # so we need to set a value for the "inverse" object
  # Run the solve() function on user-input matrix to get its inverse
  i <- solve(data)
  # Set the value of the "inverse" object to the calculated value
  x$setInverse(i)
  # Return the new "inverse" object
  i
}
