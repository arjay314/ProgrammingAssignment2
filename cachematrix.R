## makeCacheMatrix defines a set of functions that operate on a given square matrix
##      and stores results
##
## cacheSolve returns a matrix inverse, the inverse is calculated when not in cache,
##      otherwise a value is returned from the cache
##
## both functions are modifications of the makeVector and cachemean examples in the Assignment 2 doc


## makeCacheMatrix accepts a matrix as input and returns a list of functions that
##      1 - set the value of the matrix
##      2 - get the value of the matrix
##      3 - set the value of the matrix inverse
##      4 - get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) matinv <<- inverse
        getinv <- function() matinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve accepts a  list of functions (returned by makeCacheMatrix), 
##    and returns the matrix inverse
## the inverse is calculated only when it is null (not yet calculated),
##    otherwise the cached value of the inverse returned
cacheSolve <- function(x, ...) {
        matinv <- x$getinv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinv(matinv)
        matinv
}

