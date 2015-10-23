## Functions to cache inverse of matrix.
##
## Assumptions: 
##  Matrix supplied is always invertible
##
## Usage:
## > source("cachematrix.R")
## > x <- makeCacheMatrix(matrix(1:4, 2,2))
## > cacheSolve(x)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(x)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##


## This function creates a special "matrix" object,
## which is really a list containing a function to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse matrix
## 4. get the value of invese matrix

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) matrixInverse <<- inv
    getInverse <- function() matrixInverse
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created with 
## the "makeCacheMatrix" function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the "matrix" and caches it.

cacheSolve <- function(x, ...) {
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
