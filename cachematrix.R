## Put comments here that give an overall description of what your
## functions do

## when passed a matrix argument, return a list object to store the original matrix x and its inverse i;
## with get, set, getinv, setinv methods (get/set for original matrix x, getinv/setinv for inverse i)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
	    if(is.matrix(y)) {
		if( dim(x) == dim(y) && all(x == y) ) {
		   message("identical argument, leaving cache unchanged")
		} else {
		   message("new argument, resetting cache")
                   i <<- NULL
		}
		x <<- y
	    } else {
		stop("invalid argument")
	    }
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## when passed an argument which is a makeCacheMatrix() object, get its inverse, calculating it and setting it if necessary

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
