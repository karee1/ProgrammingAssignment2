## The purpose of these functions is to cache time-consuming calculations
## and therefore make the process faster. The makeCacheMatrix first caches
## the mean of a matrix and cacheSolve returns the value of the calculation.




## makeCacheMatrix is attempting to inverse the matrix, calculate its mean
## and cache it

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    s <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
        s <<- NULL
        
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    setinv <- function(solve) s <<- solve
    getmean <- function() m
    getinv <- function() s(m)
    
    list(set = set, get = get,
         setinv = setinv,
         setmean = setmean,
         getmean = getmean, 
         getinv = getinv)
}


## cacheSolve returns the mean of the matrix from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data,...)
    x$setmean(m)
    solve(m)
    m
    
}


