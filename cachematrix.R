## Written by Neeraj Bhatnagar on 2/15/2015

## This matrix inversion programs demonstartes the use of R's <<- function.
## If no valid value exists in the cache, it computes the inverse of a 
## matrix using R's solve function and caches the result.  If a valid 
## value exists, it returns the cached value.    If the value of the
## matrix itself is changed then the cached value is invalidated. 


## The function computes and stores (caches) the inverse of a matrix.
## Cached value is either null or is consistent with the current value of 
## the matrix.
## THE SUPPLIED MATRIX MUST BE INVERTIBLE.  IF NOT, TRULY BAD 
## UNPREDICTABLE THINGS MAY HAPPEN.

makeCacheMatrix <- function(x = matrix()) {
         I <- NULL
         set <- function(y) {
                 x <<- y
                 I <<- NULL
         }
         get <- function() x
         setInverse <- function(inverse) I <<- inverse
         getInverse <- function() I
         list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## If the matrix is modified since the last call, recompute and recache the 
## inverse.  Otherwise return the cached value.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
