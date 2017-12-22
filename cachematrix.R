## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.  If the inverse has already been calculated, it will find
## it in the cache and return it rather than calculating it again.

## This function creates a list containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setInv <- function(inverse) inv <<- inverse
                getInv <- function() inv
                list(set = set, get = get,
                     setInv = setInv,
                     getInv = getInv)
}

## This function computes the inverse of the special matrix created by makeCacheMatrix.
## If the inverse has already been calculated it retrieves it from the cache, otherwise 
## it will compute it and return it.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()

                #If the inverse is available retrieve it from cache
                if(!is.null(inv)) {
                    message("Getting cached data")
                    return(inv)
                } 
                #Otherwise compute and return the inverse
                else {
                    data <- x$get()
                    inv <- solve(data)
                    x$setInv(inv)
                    inv
                }
}
