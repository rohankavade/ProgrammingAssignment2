## The following R program creates the matrix and calculates the inverse of it.
## Once the inverse is calculated, it stores the value of inverse matrix, 
## and returns it when the function for the same matrix is re-executed.

## The 'makeCacheMatrix' function contains the list of functions which,
## set the matrix,
## get the matrix,
## set the inverse of matrix,
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setInv <- function(inv) {
                m <<- inv
        }
        getInv <- function() {
                m
        }
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}


## The 'cacheSolve' function first checks if the inverse of the matrix (set by the function 'makeCacheMatrix') is already calculated.
## If the inverse of the matrix already exists, it returns the stored cache value,
## Otherwise calculates the inverse and then returns it.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
         }
         else{
                mat <- x$get()
                m <- solve(mat)
                x$setInv(m)
                return(m)
        }
}
