## The following functions can be used to create a new matrix, and calculate
## its inverse matrix only in case it still has not been created.

## The function makeCacheMatrix creates an object containing both the given
## matrix and a variable for its inverse matrix. It contains the required
## functions to create and retrive both the regular matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    getinverse <- function() {
        inverse
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}


## The function cacheSolve sets and prints the inverse matrix of a matrix
## created with the makeCacheMatrix object. Before doing the computing, it
## will check whether the inverse has already been calculated. If that's 
## the case, no computing will be done.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    
    matrix_to_invert <- x$get()
    
    inverted_matrix <- solve(matrix_to_invert)
    
    x$setinverse(inverted_matrix)
    
    inverted_matrix    
}
