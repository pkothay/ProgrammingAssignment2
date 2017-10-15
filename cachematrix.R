## The below function calculates the inverse of a matrix using
## a cache if possible to accelarete computing

## This first function creates an object conatining a list of functions
## that cashes the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

        invmatrix <- NULL
        set <- function(y){
                x <<- y
                invmatrix <<- NULL
                
        }
        get <- function() x
        setsolve <- function(inv) invmatrix <<- inv
        getsolve <- function() invmatrix
        list(set =set, get = get, setsolve = setsolve,
             getsolve = getsolve)
}


## The below function calculates the inverse of a matrix
## using the above makeCacheMatrix. If the inverse has already
## calculated than the below fuction retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getsolve()
        if (!is.null(invmatrix)) {
                message("Getting chached data")
                return(invmatrix)
                
        }
        
        data <- x$get()
        invmatrix <- solve(data)
        x$setsolve(invmatrix)
        invmatrix
        
        }
