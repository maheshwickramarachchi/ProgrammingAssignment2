## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invrs <- x$getInverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setInverse(invrs)
        invrs
        ## Return a matrix that is the inverse of 'x'
}
