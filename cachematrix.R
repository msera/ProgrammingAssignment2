## Cache the inverse of a matrix.
## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve retrieves the inverse of a matrix object from its cache or 
## calculates the inverse and saves it to the cache if the cache is empty


## Create an object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Compute the inverse of a matrix created by makeCacheMatrix. If the inverse 
## has already been calculated it is retrieved from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m    
}
