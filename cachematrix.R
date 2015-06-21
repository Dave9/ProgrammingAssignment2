## This pair of functions implement an alternative to the regular solve
## function. They use the components of the makeCacheMatrix function to
## store the results of an inital invocation of the solve(matrix) function
## so that the inverse can be returned much faster on subsequent
## invocations without actually recomputing the inverse. This is 
## especially valuable for large matrices that can be very time
## consuming to compute the inverse.

## Example of use:
## >m <- matrix(c(1:2,3:4),nrow=2,byrow=TRUE) # create any invertible matrix
## >m.i <- makeCacheMatrix(m) # create the "functions" to invert it
## >cacheSolve(m.i) # calculate inverse using m.i vector instead of solve
## >cacheSolve(m.i) # quickly retrieve inverse again as needed

## The makeCacheMatrix function creates a list of functions which the
## cacheSolve function uses to calculate (setinv) or retrieve (getinv)
## the inverse of your matrix as well as functions which store (set) 
## or return (get) the matrix you want inverted. This functions parts
## can be used directly, but should properly be used with the cacheSolve
## function to ensure proper results; used directly it cannot ensure
## that the stored inverse is the inverse of the original matrix!

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function is invoked in place of the solve function.
## If the functions has been previously invoked, it will immediately
## return the stored matrix inverse; Else it will compute the inverse
## (using solve()) and store that result for the future and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
