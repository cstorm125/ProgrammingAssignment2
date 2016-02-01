
## Make a matrix that is stored in cache
makeCacheMatrix <- function(x = matrix()) {
    #set the inverse of the matrix to null
    m <- NULL
    #method to set what the matrix should be
    set <- function(y) {
        x <<- y
        #sets inverse to NULL because it needs to be recalculated
        m <<- NULL
    }
    #method to get the matrix
    get <- function() x
    #method to set the inverse of the matrix
    setsolve <- function(inver) m <<- inver
    #method to get the inverse of the matrix
    getsolve <- function() m
    #return as a list with all the methods
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Find inverse of the cache matrix
cacheSolve <- function(x, ...) {
    #get the existing inverse of the matrix
    m <- x$getsolve()
    #if it already exists, return that
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if not get the matrix for calculation
    data <- x$get()
    #calculate the inverse of matrix
    m <- solve(data, ...)
    #set the inverse of the matrix
    x$setsolve(m)
    ## Return a matrix that is the inverse of 'x'
    m
}

#Example
newmatrix<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(newmatrix)
