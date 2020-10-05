## Together, these two functions create a matrix and also cache the inverse of 
## the matrix.  This allows for avoiding extra computation of the inverse of
## a matrix if its already been cached.

## Write a short comment describing this function
##   The function below creates a list which sets a matrix, gets a matrix, sets
##   the inverse of a matrix, and gets the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- mean}
        getInverse <- function() {inv}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##   This function first checks to see if the inverse of a matrix has already 
##   been calculated - if it does, it gets the inverse from the cache without 
##   computing anything.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

