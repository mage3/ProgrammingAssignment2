## Functions for working with matricies
## Allow to cache inverse matrix operation

## Creates a list wrapper on matrix structure
## contains to get and set matrix and reverse matrix functions

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function() {
                x <<- matrix
                inv_x <<- NULL        
        }
        
        get <- function() x
        
        setInverse <- function(inv_m) {
                inv_x <<- inv_m        
        }
        
        getInverse <- function() inv_x
        
        list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## returns inverse of the matrix x
## result is cached

cacheSolve <- function(x, ...) {        
        ## Return a matrix that is the inverse of 'x'
        if (is.null(x$getInverse())) {
                x$setInverse(solve(x$get()))
        }
        
        x$getInverse()
}
