## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        setmatrix <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        getmatrix <- function() x
        setmatrixinv <- function(givenmatinv) matinv <<- givenmatinv
        getmatrixinv <- function() matinv
        
        ## return the list of functions for the matrix.
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getmatrixinv()
        if(!is.null(matinv)) {
                message("getting cached matrix inverse")
                return(matinv)
        }
        
        ## Cached value not present so calculating the inverse and caching
        data <- x$getmatrix()
        matinv <- solve(data, ...)
        x$setmatrixinv(matinv)
        matinv
}
