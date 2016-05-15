## Put comments here that give an overall description of what your
## functions do
## -----------------
## makeCacheMatrix  creates a combination of data and functions to 
##                  encapsulate the caching of the inverse function.
## cacheSolve   Uses the encapsulation of makeCacheMatrix to calculate
##              the inverse of the matrix provided. If calculated once
##              the result is cached and used for the next request.

## Write a short comment describing this function
## ------------------
## Uses a closure (environment + function) to encapsulate the original 
## matrix and the inverse of the matrix. 
## It returns a list of accessor modifier functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y;
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## Write a short comment describing this function
## ------------------
## Can work with the result of makeCacheMatrix and uses the functions
## to check for an already calculated inverse of the matrix. 
## If not yet calculated, it does so and saves the inverse to the
## result of makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
            message("hit")
            return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
