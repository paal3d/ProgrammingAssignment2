## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

## EXAMPLE USE:
## (Ensure that the matrix is non-singular - (determinant is not 0))
##
## Remember this text from the assignment description:
##
##      For this assignment, assume that the matrix
##      supplied is always invertible.
##
m1 <- rbind(c(3, 2, 5), c(2, 3, 2), c(5, 2, 4))  ## Making a matrix called m1
inv_m <- makeCacheMatrix(m1)
cacheSolve(inv_m)
## now if you do call cacheSolve(inv_m) again, you get the cached one.
cacheSolve(inv_m)
