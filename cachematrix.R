## makeCacheMatrix() function is used to set up a list that contains a matrix of x and 
## its inverse
## Example of set up a matrix:
##      p1 <- makeCacheMatrix()
##      p1$set(matrix(1:4,2,2))
## When typing p1$get(), we will get:
##           [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve() function will return the inverse of matrix x
## Just type: cacheSolve(x)
## Example:
##      p1 <- makeCacheMatrix()
##      p1$set(matrix(1:4,2,2))
##      cacheSolve(p1)
## When typing p1$getinv(), we will get:
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
## For proving, type p1$getinv()%*%p1$get(), we will get the identity matrix:
##           [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
