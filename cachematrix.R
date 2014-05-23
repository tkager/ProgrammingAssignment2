## makeCacheMatrix creates a special matrix object to cache its inverse. This is desirable as matrix inversion
## is a computationally costly process.
makeCacheMatrix <- function(x = matrix()) {
    x_inverted <- NULL
    set <- function(y) {
        x <<- y
        x_inverted <<- NULL
    }
    get <- function() x
    set_inverse<- function(inverse) x_inverted <<- inverse
    get_inverse <- function() x_inverted
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Computes the inverse of the special matrix object returned by makeCacheMatrix. If the cache already exists, this 
## function should not perform the calculation.
cacheSolve <- function(x, ...) {
    x_inverted <- x$get_inverse()
    if (!is.null(x_inverted)) {
        message("getting cached inverse matrix")
        return(x_inverted)
    } else {
        x_inverted <- solve(x$get())
        x$set_inverse(x_inverted)
        return(x_inverted)
    }
}
