## Put comments here that give an overall description of what your
## functions do

## This function returns a CacheMatrix which includes functions for setting and getting from a "cache"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
		    message("Setting matrix")
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) inv <<- solution
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function uses CacheMatrices to read from a cache if the inverse has already been solved. Solves and writes the inverse to the cache on a cache miss.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
