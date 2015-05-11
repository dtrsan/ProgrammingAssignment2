## makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mat) m <<- mat
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Calculates an inverse of matrix x returned by makeCacheMatrix.
## If the inverse is already been calculated, gets the inverse from the cache
## and skips computation. Otherwise, it calculates the inverse and cache the
## result of computation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
