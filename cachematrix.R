## This functions calculates the inverse of a matrix, but first checks 
## if it has already been calculated and stored in the cache.


## This function creates a matrix object and has 4 functions. These 
## functions are for setting and getting the matrix, and for setting and
## getting its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a matrix x. If the inverse
## has already been calculated, it retrieves it from the cache, if not, 
## it calculates it and stores it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

