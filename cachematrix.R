## makeCacheMatrix() is to be called first to store a matrix,
## and set up its inverse. Then, the inverse of that matrix
## will only be calculated once for the matrirx, subsequent
## calls to calculated the inverse will return the cached value

## makeCacheMatrix() creates objects that stores a matrix
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse object
  i <- NULL
  
  # Set the matrix object and the inverse object
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Return the matrix object
  get <- function() x
  
  # Set the inverse object
  setInverse <- function(inverse) i <<- inverse
  
  # Return the inverse object
  getInverse <- function() i
  
  # Create a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() calculates the inverse of a matrix, or
## returns the cached inverse, if it has previously been
## calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the inverse object
  i <- x$getInverse()
  
  if(!is.null(i)) {
    # If the inverse object is cached, print message,
    # return cached inverse object
    message("getting cached data")
    return(i)
  }
  
  # Otherwise, get the matrix object
  data <- x$get()
  # Call solve() to calculate the inverse of the matrix
  i <- solve(data, ...)
  # Store calculated inverse as inverse object
  x$setInverse(i)
  
  # Return inverse
  i
}
