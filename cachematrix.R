## A function for caching  a matrix inversion so it is 
## not necessary to compute it repeatedly.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               ## cached inverted matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function will check to see if the inverted matix already exists
##and if not it will compute it. 
cachemean <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) { ## checking to see if inverted matix exists.
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ##computes the inverted matirx
        x$setinv(m) ## stores the inverted matrix
        m
}