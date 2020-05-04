#This function creates a special "matrix" object that can cache its inverse.
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function( x=matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list ( set = set, get = get, 
           setsolve = setsolve,
           getsolve = getsolve)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)  {
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  #the solve function gives the inverse of a matrix
    x$setsolve(m)
    m
}