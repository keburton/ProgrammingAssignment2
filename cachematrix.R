## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. takes a matrix as an argument
## 2. gets the value of that matrix
## 3. solves the matrix to get the inverse
## 4. set the inverse of the matrix so it can be easily retrieved

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


## Checks to see if the inverse has been calculated
## If not, it computes the inverse and stores it

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