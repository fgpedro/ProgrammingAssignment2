## Calculating the inverse of a matrix can be a costly operation.  The two functions below 
## calculate the inverse of a matrix and cache the inverse of that matrix into a special
## matrix such that when a matrix inversion calculation is performed, the function
## checks first if a cache is available so that the inversion is no longer calculated.

## The makeCacheMatrix creates a special matrix, which performs the following functions:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

	cachematrix <- NULL

        set <- function(y) {
                x <<- y
                cachematrix <<- NULL
        }

        get <- function() x

        setmatrix <- function(matrixinput) cachematrix <<- matrixinput

        getmatrix <- function() cachematrix

        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## The cacheSolve function calculates the inverse of a matrix created
## within the makeCacheMatrix function.  The function checks if the 
## inverse matrix has already been calculated.  If it was, it gets
## the inverse matrix from the cache and skips the calculation.  If
## the inverse is not in the cache, the function caculates the inverse
## and sets the inverse matrix in the cache through the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getmatrix()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- solve(data, ...)
	        
        x$setmatrix(m)

        m
}
