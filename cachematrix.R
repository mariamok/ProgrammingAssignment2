## Given a matrix whose inverse is solvable, the following two functions are used to return the value of
## the inverted matrix

## invertible matrix used for testing:   m<-matrix(c(1,2,3,0,1,4,5,6,0), 3,3)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(x) m <<- x
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The following function returns the inverse of a matrix
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it inverts the matrix and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
