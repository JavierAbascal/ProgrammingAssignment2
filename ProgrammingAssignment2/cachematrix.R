## Javier, 23-11-2014

## This file contains two special functions to create a special matrix and obtain
## its inverse, Caching the inverse of a Matrix

## makeCacheMatrix receives a matrix and create a new special one that can be used
## with the function cacheSolve. This special matrix is a list with 4 functions: 
## set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve receives a special matrix created with makeCacheMatrix and return its inverse.
## cacheSolve only calculate the inverse if the special matrix doesn't have it already in cache
## (saving it in the special matrix, once calculated)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
