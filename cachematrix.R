## Matrix inversion can be costly and computationally timeconsuming. This file
## uses a special matrix object that stores the inverse of a matrix so it can
## later be used without doing the inversion again but just accessing the stored
## result from before.
## Function 'makeCacheMatrix' is used to set up the special matrix object
## Function 'cacheSolve' computes the inverse if that hasn't been done before
## and stores the results in the special matrix object

## EXAMPLE:
## > M <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2, ncol=2))
## > M$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(M)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > M$getinverse()
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## The function 'makeCacheMatrix' is the generator for a special matrix object.
## These objects have custom functions which can be used to effectively get and 
## set the value of the matrix and its inversex.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## The function 'cacheSolve' checks if for an object that was created with the 
## makeCacheMatrix function the inverse was already computed. If not, then the
## function computes the inverse and assigns it to the object. The function makes
## use of the internal functions (getinverse & setinverse) that are provided 
## by the object.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    return(m)
}
