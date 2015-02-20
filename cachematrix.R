## Two functions to create a matrix object that can cache a matrix and its 
## inverse. The latter is only calcualted if it hasn't been done so previously 
## or the matrix has changed.

## Function to create an object that caches a matrix and its inverse. Contains
## functions to set and get the matrix and its inverse. When the matrix is
## changed the inverse is set to NULL.

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


## Function to calculate the matrix inverse for the object created with above 
## function. If inverse has been calucated previously it retrieves it from the
## cache.

cacheSolve <- function(x, ...) {
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