cachemean <- function(x, ...){
    m <- x$getmean()
    if(!is.null(m)){
        message("getting cached data")
        return()
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}