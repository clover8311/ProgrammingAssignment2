## functions that cache the inverse of a matrix if it is already computed. If not, setInversed will compute and save the 
## value in cache. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    set <- function(y) {
      x <<- y;
      inversedMatrix <<- NULL;
    }
    get  <- function() x
    setInversed <- function(solve) inversedMatrix <<- solve
    getInversed <- function() inversedMatrix
    # save the values in a list
    list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversedMatrix <- x$getInversed()
    #if the matrix already computed, print message and get the value
    if(!is.null(inversedMatrix)) {
        message("getting cached matrix data")
        return(inversedMatrix)
    }
    # if the matrix didn't computed yet, compute it.
    data <- x$get()
    inversedMatrix <- solve(data, ...)
    x$setInversed(inversedMatrix)
    inversedMatrix
}
