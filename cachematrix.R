## A pair of functions that allows us to cache the inverse of a matrix
## so we do not have to recalculate it every time we need it.

## makes a list of functions out of a matrix. included are a getter 
## and setter for the matrix itself and a getter and setter for the 
## solution of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## takes a list created by makeCacheMatrix and returns the cached
## inverse if it has been calculated already, and solves it if it hasn't.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
