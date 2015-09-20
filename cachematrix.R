## The following functions can be used in processing large Matrix data, in case we need to
## find inverse of the Matrices many a times. This functions caches the inverse of the Matrix 
##data and only get the inverse if already calculated

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1.set the value of the Matrix
# 2.get the value of the Matrix
# 3.set the value of the inverse of the Matrix
# 4.get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    iv<- NULL
    set <- function(y){
        x<<- y
        iv<<- NULL
    }
    get<- function() x
    setsolve <- function(solve) iv <<- solve
    getsolve <- function() iv
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}

## The following function calculates the inverse of the special "matrix" 
#  created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of 
# the inverse Matrix in the cache via the setsolve function.



cacheSolve <- function(x, ...) {
    iv <- x$getsolve()   #iv denotes inverse of the matrix
    if(!is.null(iv)){
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setsolve(iv)
    iv #returns the inverse of the Matrix
}
