## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inv <- NULL
  
  # Method to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse matrix in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse matrix from the cache
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




## this is the function for cache solve

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inv <- NULL
  
  # Method to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse matrix in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse matrix from the cache
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the inverse for future use
  inv
}


