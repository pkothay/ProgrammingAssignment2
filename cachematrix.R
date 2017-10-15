## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
