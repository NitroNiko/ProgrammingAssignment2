## The two following function are calculation time saver for inverting a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## this function creates a matrix object which can cache its inverse
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    setSolve <- function(inv) inverse <<- inv
    getSolve <- function() inverse
    list(set = set, get = get, 
         setSolve = setSolve,
         getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from cach or calculation
    inverse <- x$getSolve()
    if (!is.null(inverse)){
        message("getting inverse from cache")
        return(inverse)
    }
    mx <- x$get()
    inverse <- solve(mx)
    x$setSolve(inverse)
    inverse
}
