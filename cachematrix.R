# Matrix inversion using caching

# Create custom matrix object to enable caching

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # clear cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        } # set matrix
        get <- function() x # return matrix
        setinverse <- function(inverse) m <<- inverse # set inverse
        getinverse <- function() m # return inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Perform inversion if the result is not already cached

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # get cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } # return cached inverse if it exists
        data <- x$get() # get matrix
        m <- solve(data, ...) # solve matrix
        x$setinverse(m) # cache inverse
        m # return inverse
}

