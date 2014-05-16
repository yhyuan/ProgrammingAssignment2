## This module contains two functions. The first function named makeCacheMatrix aims to create a special
## "matrix", which is really a list containing a function to 
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse of the matrix
##     4. get the value of the inverse of the matrix
## The second function calcualtes the inverse of the special "matrix" created with the first function.
## First, it checks whether the inverse has already been calculated. If so, it gets the mean from the cache
## and skip the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setInverse method.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
		inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
