makeMatrix <- function(x = matrix()) {
        t <- NULL
        set <- function(y) {
                mat <<- y
                t <<- NULL
        }
        get <- function() mat
        setinverse <- function(matrix.inverse) t <<- matrix.inverse
        getinverse <- function() t
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheinverse <- function(x, ...) {
        m <- getinverse[1]
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- get[1]
        m <- matrix.inverse(data, ...)
        setinverse[1]
        m
}