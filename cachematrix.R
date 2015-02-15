## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 

## The following functions cache the inverse of a matrix.

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mat.inv <- NULL
        set <- function(y) {
                x <<- y
                mat.inv <<- NULL
        }
        get <- function() x
        setmatinv <- function(solve) mat.inv <<- solve
        getmatinv <- function() mat.inv
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## cacheSolve
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        mat.inv <- x$getmatinv()
        if(!is.null(mat.inv)) {
                message("getting cached matrix")
                return(mat.inv)
        }
        data <- x$get()
        mat.inv <- solve(data, ...)
        x$setmatinv(mat.inv)
        mat.inv
}