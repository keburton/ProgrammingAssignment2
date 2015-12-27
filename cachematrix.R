## Programming Assignment 2

## Takes a matrix as an argument and 
## inv set as NULL creates a open slot for the inverse to be stored.
## The list allows set, get, setinv, and getinv to be called for 
## whatever matrix is the input of the function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

## This function computes the inverse of the matrix above.
## It checks to see if the inverse has been calculated and
## if not, it computes the inverse and stores it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}