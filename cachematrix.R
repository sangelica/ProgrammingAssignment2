## File contains 2 functions:
## 1. makeCacheMatrix - creates an object with capability to
##                      get and set/cache the inverse of
##                      based on an input matrix
## 2. cacheSolve - gets the inverse of a matrix from the cache
##                 if available, otherwise computes for it and
##                 caches it

## makeCacheMatrix
## description: function caching the inverse of a matrix
## parameter  : orig_matrix - input invertible matrix
## output     : list object containing functions to set/get
##              the input matrix and set/get inverse matrix

makeCacheMatrix <- function(orig_matrix = matrix()) {
    inv_matrix <- NULL
    set <- function(input_matrix) {
        orig_matrix <<- input_matrix
        inv_matrix <<- NULL
    }
    get <- function() orig_matrix
    setinverse <- function(input_inv_matrix) inv_matrix <<- input_inv_matrix
    getinverse <- function() inv_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## description: function that will return the cached inverse of
##              matrix if available, computes for the inverse
##              otherwise using "solve function"
## parameter  : list object output of makeCacheMatrix
## output     : inverse of input matrix

cacheSolve <- function(matrix_obj, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- matrix_obj$getinverse()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    orig_data <- matrix_obj$get()
    inv_matrix <- solve(orig_data)
    matrix_obj$setinverse(inv_matrix)
    inv_matrix
}
