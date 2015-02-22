# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly. Below are two functions that are used 
# to create a special object that stores a matrix and cache's its inverse. 

# makeCacheMatrix:This function creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set
         ,get = get
         ,setinverse = setinverse
         ,getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}