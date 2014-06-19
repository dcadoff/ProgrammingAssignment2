## This R script contains two functions, makeCacheMatrix and cacheSolve, that when
## used together, will accept any invertible, square matrix and solve for the inverse
## matrix, display the results and cache the inverse in memory. The caching is
## designed to save processing time by preventing a matrix that's the same as
## the previously submitted matrix from being recalculated. 

## makeCacheMatrix creates a list of four (4) functions:
## The functions are:
## set - which sets the value of a matrix
## get - gets the value of a matrix
## setinverse - sets the inverse value of a matrix
## getinverse - gets the inverse value of a matrix
## These functions are designed to be called from other functions; hence lexical
## scoping is needed in the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve does two things:
## First, it checks to see if the inverse of a square matrix has been cached in memory.
## Then, if the inverse has been cached, the solve is displayed.
## If the square matrix is new, then the function solves for the inverse, displays
## the new results and, finally, caches the new inverse in memory.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
