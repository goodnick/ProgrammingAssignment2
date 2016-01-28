## A set of functions that allow efficient computation of the inverse of a matrix
## through caching.
## Example:
##      x <- matrix(sample(1:9), 3, 3)
##      y <- makeCacheMatrix(x)    ## New x with methods to cache inverse
##      cacheSolve(y) == solve(y)
##      ## cacheSolved will be more efficient on subsequent calls

## makeCacheMatrix - adds a wrapper around a matrix with new accessors and the
## ability to cache the inverse
## Wrapper Methods:
##      get - returns the matrix
##      set - sets the matrix.  Clears the cached value.
##      getinverse - gets the cached inverse. NULL if it has not been set
##      setinverse - sets the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  ## cached inverse value
    set <- function(y) {
        x <<- y
        inverse <<- NULL    ## Clear the cache
    }
    get <- function() x
    setinverse <- function(newInverse) inverse <<- newInverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - returns the inverse of the given cacheMatrix (Created through
## makeCacheMatrix above) and caches the value for efficient reuse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()

    ## returned cached value if it exists
    if(!is.null(inverse)) {
        return(inverse)
    }

    ## else calculate inverse and set the cache on x
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)

    ## Return a matrix that is the inverse of 'x'
    inverse
}
