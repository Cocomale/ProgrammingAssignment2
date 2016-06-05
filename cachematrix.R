## Creating a special matrix for inverse and caching it

## Special Matrix

makeCacheMatrix <- function(x = matrix()) {I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## caching the inverse if it exists

cacheSolve <- function(x, ...) {I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}
