## Below are two functions for increases efficiency when calculating inverses 
## of many 2x2 matrices. If an inverse has already been calculated before
## we get the cached version instead of calculating it again

## The function below takes a matrix as an argument and returns a list of 
## functions 

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


## The function below takes in a matrix and checks whether the inverse of that
## matrix have been calculated previously or not. 
## If it has been calculated previously, it returns the old inverse that is 
## being grabbed by x$getinverse()
## If not previously calculated, the Sovle() function is used to calculate it 
## and the inverse matrix gets set in x and then returned.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m   ## Return a matrix that is the inverse of 'x'
}
