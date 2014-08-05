## A couple of functions for caching inverted matrices

## Creates a cacheable matrix for the cacheSolve()
## function, which sets/gets cached values
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.inverse <- function(solve) m <<- solve
        get.inverse <- function() m
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## Computes the inverse of the matrix created in makeCacheMatrix(),
## Returns the cached inverse if possible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get.inverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set.inverse(m)
        m
}
