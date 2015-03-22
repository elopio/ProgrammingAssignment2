## Programming Assignment #2 for the Coursera R Programming course.
## Cache the inverse of a matrix so it has to be calculated only once.

## Wrap a matrix with methods to set and get the matrix data, and set and get
## the matrix inverse. The matrix data and its inverse are stored in the
## parent environment.
## Parameter x: a matrix that can be inverted.
## Returns a list with the methods to get and set the matrix data, and get and
## set the inverse of the matrix, in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set.inverse <- function(inv) inverse <<- inv
    get.inverse <- function() inverse
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Return the inverse of a matrix.
## If the inverse has not been calculated, it is stored in the parent
## environment so it doesn't have to be calculated again. If the inverse was
## already calculated, this function just returns the previously calculated
## value.
## Parameter x: a wrapper for a matrix as returned by the makeCacheMatrix
## function.

cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set.inverse(inverse)
    inverse
}
