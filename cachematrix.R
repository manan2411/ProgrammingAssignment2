## The following functions create a special object that stores a matrix and caches its inverse.
## This helps in avoiding redundant calculations by retrieving the inverse from the cache when needed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list containing the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Check if inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse if not cached
  data <- x$get()
  inv <- solve(data, ...)  # Compute inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the computed inverse
}

## Example Usage:
# matrix_data <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
# cached_matrix <- makeCacheMatrix(matrix_data)
# cacheSolve(cached_matrix)  # Computes and caches the inverse
# cacheSolve(cached_matrix)  # Retrieves the cached inverse
