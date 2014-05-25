## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(b) m <<- b
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        # found cached inverted matrix - use it
        message("returning cached value")
        return(m)
    } else {
        # Get inverse of matrix
        # Excepton handling - see http://www1.maths.lth.se/help/R/ExceptionHandlingInR/
        tryCatch({
            data <- x$get()
            m <- solve(data,...)
            x$setinverse(m)
            return(m)
        }, error = function(ex) {
            cat("An error was detected.\n")
            print(ex)
        }) # tryCatch()
    } # else !is.null
}
