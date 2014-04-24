## creates a special "matrix" object that can cache its inverse
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## takes an arg x of type matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    ## returns a list of 4 items (which are actually functions)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## query the cache
    m <- x$getInverse()
    
    ## if there is a cache (ie not NULL), don't compute, just return
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise (no cache), compute and save the results back to x's cache
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    
    ## return the result
    m
}
