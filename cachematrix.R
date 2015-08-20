## This is an assignment for the R Programming course on Coursera

## The first function creates a "special" matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) i <<- inv
     getinverse <- function() {
          i
     }
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The second function computes the inverse of the "special" matrix
## returned by the first function. If the inverse has already been calculated (and
## the matrix has not changed), then it should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data...")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
