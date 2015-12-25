## These functions allow for the storage of a matrix and the calculation of
## its inverse.  Methods are provided for setting  and retrieving these values
## and for caching the inverse matrix to save computation time.

## makeCacheMatrix initializes the matrix and creates an object for the inverse
## matrix upon its calculation. It returns a list of four functions for setting
## and retrieving those values

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve will return the cached inverse matrix if it exists.  It it does
## not already exist, it calculate the inverse and cache it, then return the 
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
