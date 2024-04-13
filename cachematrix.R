## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize cache
    cache <- NULL
    
    # Set the matrix
    set <- function(matrix) {
        x <<- matrix
        cache <<- NULL
    }
    
    # Get the matrix
    get <- function() {
        x
    }
    
    # Set the inverse of the matrix
    setInverse <- function(inverse) {
        cache <<- inverse
    }
    
    # Get the inverse of the matrix
    getInverse <- function() {
        cache
    }
    
    # Return list of functions
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Computes the inverse of the special matrix object and caches it
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    
    # If not cached, compute the inverse
    data <- x$get()
    inverse <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inverse)
    
    # Return the inverse
    inverse
}
