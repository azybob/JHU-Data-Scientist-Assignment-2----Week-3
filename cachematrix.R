## Functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        originalMatrix <<- y
        inverseMatrix <<- NULL
    }
    
    # Functions for getting and setting cached inv. matrix value
    get <- function() originalMatrix
    # Using 'solve()' function to get the inverted matrix
    setinverse <- function(solve) inverseMatrix <<- solve
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheMatrix, ...) {
    inverseMatrix <- cacheMatrix$getinverse()
    # Is there any cached matrix?
    # If yes, call it.
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    # If not, create the inverted matrix.
    data <- cacheMatrix$get()
    inverseMatrix <- solve(data, ...)
    cacheMatrix$setinverse(inverseMatrix)
    inverseMatrix
    ## Return a matrix that is the inverse of original matrix
}
