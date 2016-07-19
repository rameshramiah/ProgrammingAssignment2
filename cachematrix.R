## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following are a pair of functions that creates a special "matrix" object
## and cache it's inverse which can retrived if the inverse has been calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has alreadybeen calculated then the
## cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
