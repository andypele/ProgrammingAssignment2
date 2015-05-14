## makeCacheMatrix and cacheSolve are used to efficiently compute the inverse
## of a matrix and to cache that inverse along with the matrix so that it can
## be retrieved without further computation once it has been solved a single time

## Return a list of four functions associated with a matrix, x: set(), get(), setinverse (), and getinverse()
## Those four functions can be thought of as the methods of an 'x' object, together with its data (the original matrix x
## and its inverse.)
##
## Until an inverse of x is computed, getinverse() returns NULL. set() can be used to "fill" an empty matrix.
## cacheSolve() below calls getinverse() to see if an inverse must be computed and if so calls setinverse()
## to cache the computed inverse with the 'x' object created by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
 	inverse <- NULL			# NULL is the default value for the inverse until an inverse is computed
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
## 'x' is an object that has been created with the makeCacheMatrix function so 
## we can first check to see if the inverse has already been computed

cacheSolve <- function(x, ...) {
        
	inv <- x$getinverse()
        if(!is.null(inv)) {				# inverse has already been computed
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
