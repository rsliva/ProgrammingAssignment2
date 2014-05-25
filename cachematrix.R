# cachematrix.R 
# Contains two functions used to invert a matix, and to cache the inverse of the matrix for later use.

# makeCacheMatrix: creates a matrix object that holds a cached version of it's inverse
#                  and includes helper methods to get and set both the matrix and matrix inverse.
# x: the matrix to be inverted
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


# cacheSolve: function to use solve() function to get the inverse of a matrix. If a cached version
#             of the inverse already exists in the object then that is returned instead.
#             Some basic error handling is used if the matrix is not invertible.
#   x: matrix object with matrix to be inverted. The matrix object is created
#      with builtin caching support by using the makeCacheMatix function above.
# ...: additional paramters to be passed to the solve() function.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
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
