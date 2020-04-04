## following functions compute and cache the inverse of matrix

## cteating of a "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinver <- function(inverse) inver <<- inverse
    getinver <- function() inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}


## computes/ retrieving from cache the inverse of the matrix
cacheSolve <- function(x, ...) {
            inver <- x$getinver()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinver(inver)
    inver
}
