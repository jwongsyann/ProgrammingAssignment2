## makeCacheMatrix takes a matrix and and makes a list of four functions set,get,setinverse and getinverse
## cacheSolve gets the cached inverse of a matrix if it already is calculated or gets the inverse of a matrix
## if it has not been calculated yet.

## Takes a matrix and makes a list of four functions which allows the matrix to be manipulated

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse = matrix()) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Gets the cached inverse of a matrix if it has been calculated before or solves a matrix to obtain its inverse using
## solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
