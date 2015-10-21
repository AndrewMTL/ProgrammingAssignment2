## Here we are calculating the inverse of a matrix.
## Once calculated, we will cache the inverse so that,
## if we need it again, we can retrieve it and avoid
## the need to re-calculate it.

## Create a list containing functions to:
## 1. Set a new matrix (x) to be inversed - x$set(matrix)
## 2. Get the stored matrix to be inversed - x$get()
## 3. Set the inverse of the stored matrix - x$setinverse(inverse)
## 4. Get the inverse of the stored matrix - x$getinverse()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the inverse is already cached, this function will look it up and
## return it. Otherwise it will calculate it, cache it and return it.

cacheSolve <- function(x, ...) {
    # Check to see if the inverse is already cached and, if so, return it
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # If it is not cached, calculate it, store it in the cache and return it
    data <- x$get()
    new_inverse <- solve(data)
    x$setinverse(new_inverse)
    new_inverse
}
