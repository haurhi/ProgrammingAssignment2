## Based on the scoping rules of R programming, a separation of taking and computing 
## value will be realized in the funciton to save the computation effort.This function 
## is able to cache the inverse of a matrix if the original one is not changing.

## A set of functions with setting an initial matrix, getting an initial matrix,
## setting the inverse matirx and getting the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) x <<- y
    get <- function() x
    setinverse <- function(inverse) r <<- inverse
    getinverse <- function() r
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function is to calculate the inverse matrix if the result has not been found.
## Otherwise, it just takes the result from the cache and skips the computation.

cacheSolve <- function(x, ...) {
       r <- x$getinverse()
       if(!is.null(r)) {
           message("getting cached data")
           return(r)
       }
       matrix <- x$get()
       r <- solve(matrix, ...)
       x$setinverse(r)
       r
}
