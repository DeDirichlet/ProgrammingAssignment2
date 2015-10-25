## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cacheInverse <- NULL
        set <- function(y) {
                x <<- y
                cacheInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cacheInverse <<- inverse
        getInverse <- function() cacheInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFun <- x$getInverse()
        if(!is.null(invFun)) {
                message("getting cached data")
                return(invFunc)
        }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}