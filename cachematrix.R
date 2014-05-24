## Here is a pair of functions that cache the inverse of a matrix.

## This makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL                                        ## "i" is the variable for the inverse matrix
        
        setmatrix <- function(y) {                       ## "setmatrix" sets the value of the matrix
                x <<- y
                i <<- NULL
        }
        
        getmatrix <- function() x                        ## "getmatrix" returns the value of the matrix "x"
        
        setinverse <- function(inverse) i <<- inverse    ## "setinverse" calculates the inverse of the matrix
                                                         ## and stores the inverse matrix in the variable "i"
        
        getinverse <- function () i                      ## "getinverse" returns the value of the inverse matrix
        
        list(setmatrix = setmatrix,                      ## This is the created vector (list) that contains
             getmatrix = getmatrix,                      ## all the functions.
             setinverse = setinverse,
             getinverse = getinverse)
}

## This cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then this cacheSolve function will retrieve the
## inverse from the cache. Otherwise, cacheSolve will compute the inverse and
## cache it via the setinverse function.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()                              ## "i" returns a matrix that is the inverse of "x"
        
        if(!is.null(i)) {                                ## First the function checks if the inverse matrix
                message("getting cached data")           ## is stored in the cache.
                return(i)                                ## If inverse is stored: function returns inverse matrix.
        }
        
        data <- x$getmatrix()                            ## If inverse is not stored: function gets the matrix to
        i <- solve(data, ...)                            ## calculate inverse matrix
        x$setinverse(i)                                  ## calls "setinverse" function to store inverse in the cache
        i                                                ## then returns the inverse matrix
}

