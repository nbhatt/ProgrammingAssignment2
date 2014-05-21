## The following functions will calculate and cache the inverse of a matrix


## makeCacheMatrix will create a special matrix object.
## This function returns a list of functions that get be performed on that matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function() inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix returned in 
## makeCacheMatrix above. 
## If the inverse has already been calculated and there has been no change 
## to the matrix, the function will return the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                inv
        }
        data <- x$get()
        x$setinv()
}
