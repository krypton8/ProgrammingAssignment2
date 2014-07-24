## The makeCacheMatrix fucntion creates a list object of functions used to set
## & get the X matrix, and to set & get the inverse of the X matrix. It does
## not actually compute the inverse matrix.
## The cacheSolve computes the inverse matrix and sets it using the functions
## provided by makeCacheMatrix above.

## Creates a list object that points to the four functions set, get, setinv, &
## getinv. Initializes inverse matrix to NULL.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <-    function(solve) x_inv <<- solve
    getinv <- function() x_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Accepts the list object created by the makeCacheMatrix as an argument.
## It looks for a cached copy of the inverse of matrix X, if not found, it
## computes the inverse using the solve function and sets it using the setinv()
## function provided in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached inverse matrix...")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    x_inv
}
