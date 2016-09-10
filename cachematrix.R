# A pair of functions that work together to provide a method
# of storing gettting the inverse of a matrix, either
# by using the `solve` function, or by retrieving the result of
# an earlier call to `solve`.  Matrices are represented as a list
# of functions that get and set the base representation of the matrix and
# the base representation of its inverse.
# Functions in the list are as follows:
# 
# set(X)            Sets the value of the matrix and clears
#                   the inverse value in cache
# 
# get()             Gets the value of the matrix
# 
# setinverse(Xinv)  Sets a value for the inverse matrix in cache
#                   (The user is responsible for setting the correct value)
# 
# getinverse()      Gets the stored value of the inverse matrix


## Create the list of functions representing X
makeCacheMatrix <- function(X = matrix()) {
    Xinv <- NULL
    set <- function(Xset) {
        X <<- Xset
        Xinv <<- NULL
    }
    get <- function() X
    setinverse <- function(inv) Xinv <<- inv
    getinverse <- function() Xinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a matrix constructed from an output
## of the function makeCacheMatrix.  The inverse is computed and 
## stored if it doesn't alread exist.

cacheSolve <- function(cacheMat, ...) {
    Xinv <- cacheMat$getinverse()
    if(!is.null(Xinv)) {
        message("getting cached data")
        return(Xinv)
    }
    X <- cacheMat$get()
    Xinv <- solve(X, diag(dim(X)[[1]]), ...)
    # diag(dim(X)[[1]]) gives the identity matrix as the
    # b input to `solve` thereby allowing ... of `cacheSolve`
    # to correspond to ... of `solve`
    cacheMat$setinverse(Xinv)
    Xinv
}
