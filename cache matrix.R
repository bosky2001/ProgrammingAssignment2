#use library(matlib) to access inv(),Inverse() functions
#function to cache matrix inverses
makeMatrix <- function(x = matrix()) {
        #making a null matrix to store inverse value
        t <- NULL
        #
        set <- function(matrix) {
                mat <<- matrix
                t <<- NULL
        }
        #retrieving the matrix
        get <- function() mat
        #assigning the inverse matrix
        setinv <- function(inverse) t <<- inverse
        #retrieving it 
        getinv <- function() t
        #list of all methods
        list(set = set, get = get,
             setinv = setinv,
             getinv= getinv)
}
#function to retrieve inverse of matrix if in cache ,if not, this computes it 
cacheinverse <- function(x, ...) {
        #returning the matrix inverse if there
        m <- x$getinv()
        #if m is'nt null, i.e inverse is in cache and returning the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #getting matrix
        data <- x$get()
        #finding the matrix inverse
        m <- inv(data, ...) # even <-Inverse(data,...) and <-solve(data) %*% data
        x$setinv(m)
        m
}
