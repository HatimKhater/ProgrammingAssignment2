## Put comments here that give an overall description of what your
## functions do

The makeCacheMatrix function is designed to create a special type of matrix object that not only stores the matrix but also has the ability to cache its inverse for efficient computations.

The cacheSolve function computes the inverse of a matrix, taking advantage of the caching capabilities provided by the matrix object created with makeCacheMatrix

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize an object to store the inverse matrix, initially set to NULL.
  inv = NULL
  
  # function 'set' sets the matrix value and resets the cached inverse to NULL.
  set = function(matrix) { x <<- matrix; inv <<- NULL }
  
  # function 'get' retrieves the matrix value.
  get = function() x
  
  # this function sets the cached inverse to a specified value.
  setInverse = function(inverse) inv <<- inverse
  
  # this function retrieves the cached inverse.
  getInverse = function() inv
  
  # Return a list of functions that can be used to interact with the created "matrix" object.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function -----------------


cacheSolve <- function(x, ...) {
    
    # Attempt to retrieve the cached inverse from the x
    inverse = x$getInverse()
    
    # Check if the cached inverse is available
    if (!is.null(inverse)) { 
        # If cached inverse exists, print a message and return the cached value
        message("getting cached inverse.")
        return(inverse) 
    }
    
    # If the cached inverse is not available, calculate it using the solve() function
    matrix = x$get()
    inverse = solve(matrix, ...)
    
    # Cache the calculated inverse for future use
    x$setInverse(inverse)
    
    # Return the inverse
    return(inverse)
}
