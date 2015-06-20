## In order to reduce the computational load of solving for
## the inverse of a matrix, the following functions are
## used to create a special version of a matrix that is
## able to cache its inverse.

## makeCacheMatrix is responsible for creating an object
## that stores a matrix, and its inverse.
## get and set are added as getter and
## setter functions for the original matrix.
## getinverse and setinverse are added as getter and
## setter functions for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse attribute as NULL (empty)
    inv <- NULL
    # Create setter function that sets a new value for the
    # primary matrix and empties the cache.
    # Note that the "super assignment" operator is used to
    # set the attributes on the cache matrix object rather
    # than inside the scope of the setter function.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Create getter function that retrieves the primary matrix
    get <- function() x
    # Create setter function for the inverse matrix
    setinverse <- function(i) inv <<- i
    # Create getter function that retrieves the inverse matrix
    getinverse <- function() inv
    # Return the set of functions as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve accepts as its primary argument any matrix
## created by makeCacheMatrix.
## It returns the cached inverse if available, 
## or calculates the inverse otherwise.
## Any further arguments are passed to the solve method

cacheSolve <- function(x, ...) {
    # Get the cached inverse and return it if it exists
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Cache not set, so get the original matrix
    data <- x$get()
    # Solve it
    inv <- solve(data, ...)
    # Set the cache for next time
    x$setinverse(inv)
    # Return the inverse matrix
    inv
}
