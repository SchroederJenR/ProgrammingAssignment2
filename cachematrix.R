## Creates a matrix object that can cache its inverse
## Computes the matrix inverse or retrieves it if already calculated

## Creates an object -- "matrix" of functions

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
      x <<- y
      v <<- NULL
    }
    get <- function() x
    setinv <- function(solve) v <<- solve
    getinv <- function() v
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    v <- x$getinv()
    if(!is.null(v)) {
      message("getting cached data")
      return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinv(v)
    v
}
