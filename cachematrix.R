# Creates an object that represents a matrix with a cache-able inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(newx) {
        x <<- newx
        # Invalidate cache because the matrix has changed
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) {
        inv <<- newinv
    }
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# Returns the inverse of a matrix, cached for efficiency
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv))
        return(inv)
    # else:
    print("value not cached - calculating")
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
