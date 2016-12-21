## These functions shall create an object from square invertible matrix, and
## calculate the inverse of the special matrix object. The result of the 
## calculation shall be cached. In future iterations, if matrix is unchanged 
## and inverse already calculated, then function shall pull the inverse from 
## the cache.

## The function makeCacheMatrix can be used to create an object.
## Ex: myMatrix <- makeCacheMatrix(x), where x is a presumably invertable matrix.
## The function initiates an object inverse as null, and defines the four
## functions set (can be used to reset the argument to makeCacheMatrix without
## creating an additional object), get, set_inverse and get_inverse (all used by
## the next function, cacheSolve. The result of makeCacheMatrixis a list of the
## four functions.

makeCacheMatrix <- function(x = matrix()) {
                inverse <-  NULL
                set <- function(y){
                        x <<- y
                        inverse <<- NULL
                }
                get <- function() x
                set_inverse <- function(solve) inverse <<- solve
                get_inverse <- function() inverse
                list(set = set, get = get, set_inverse = set_inverse,
                     get_inverse = get_inverse)
}


## cacheSolve takes as an argument the object created by makeCacheMatrix
## (myMatrix). It checks to see if object inverse is not null, meaning that the
## inverse of the object has already been calculated and is stored as the object
## inverse. If that condition is TRUE, then the function displays the message "getting
## cached data" and returns the object inverse. If the condition is FALSE, 
## cacheSolve calls the get() function defined by makeCacheMatrix and calculates
## the inverse of the matrix object. Then the function calls set_inverse() and 
## stores the result as object inverse. CacheSolve then returns the object
## inverse.

cacheSolve <- function(x, ...) {
                inverse <- x$get_inverse()
                if(!is.null(inverse)) {
                        message("getting cached data")
                        return(inverse)
                }
                data <- x$get()
                inverse <- solve(data, ...)
                x$set_inverse(inverse)
                inverse
}
