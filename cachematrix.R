## Computing an inverse of a matrix and saving it in cache. When again asked 
## for the inverse, it is taken out of cache and not computed again

## provides 4 functions in itself to compute inverse-values and to provide given
## values. set and get...
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cachSolve checks whether an inverse value is already available in cache
## if not it asks to compute the inverse and saves sets it. 
## functions are used from makeCacheMatrix-function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting data from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
