## A makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## A cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
##

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

		 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}


## The following function calculates the inversion of the special "matrixr" created with the above function.
## However, it first checks to see if the inversion has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inversion of the data and sets the value of the inversion in the cache via 
## the setinversion function.

cacheSolve <- function(x, ...) {

   m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
