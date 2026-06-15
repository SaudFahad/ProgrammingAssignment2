## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions: set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

