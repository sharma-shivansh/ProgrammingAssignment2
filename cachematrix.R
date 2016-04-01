## Below functions calculates the inverse of a matrix 
## and caches the matrix for subsequent function calls

## Creating caching matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverted <- function(invert) inv <<- invert
    getinverted <- function() inv
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## Inverting the matrix if inverted matrix isn't available in cache

cacheSolve <- function(x) {
    inv <- x$getinverted()
    if(!is.null(inv) && !is.na(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverted(inv)
    inv
}
