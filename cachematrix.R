## makeCacheMatrix creates a data structure that contains a matrix
## data and its inverse. The return is the list of operations on
## this data structure. cacheSolve returns the calculated or retrived
## cache of a data structure defined by makeCacheMatrix.

## Create and return a special matrix that contains its cached
## inverse if it has been already calculated. The return list
## is a list of functions for setting and getting the matrix
## or its inverse value.

makeCacheMatrix <- function(x = matrix()) {
    ## Return a cachematrix version of the input matrix.
    ## The return is a list of functions allowing to get and set
    ## the matrix or its cached inverse.
    
    # The inverse of the matrix.
    inverse <- NULL
    # Fuction to set the matrix data.
    set <- function(m) {
        x <<- m
        # Reset the inverse as the matrix data had changed.
        inverse <<- NULL        
    }
    # Function to get the matrix data.
    get <- function() x
    # Function to set the inverse of the matrix.
    setinverse <- function(inv_m) inverse <<- inv_m
    # Function to get the inverse of the matrix.
    getinverse <- function() inverse
    # Return the list of operations on this matrix data structure.
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of matrix x, which is a data
## structure that has operations and a cached version of the
## inverse if it has already been computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x is a cached matrix datastructure constructed by makeCacheMatrix
    # Try to get the inverse from the cache first.
    inv <- x$getinverse()
    # If inverse is not computed, compute it and store it
    # in the cache.
    if(is.null(inv)) {
        # Get the matrix data.
        matrix_data <- x$get()
        # Assume the input is correct
        inv <- solve(matrix_data)
        # Set the cache
        x$setinverse(inv)
    }
    # Return the inverse of matrix x.
    inv
}