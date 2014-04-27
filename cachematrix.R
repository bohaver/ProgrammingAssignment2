# cachematrix will store the inverse of a matrix in memory along
# with the matrix. This is useful when the inverse of a matrix may
# be repeatedly calculated on the same large matrix.


# Matrix wrapper that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# A "Solve" function that will use a cached inverse if available.
# Must be used with a cache matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached inverse")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
