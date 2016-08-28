## makeCacheMatrix is a function that creates a special matrix object that can cache its inverse "i"
## assumes x = matrix is an invertible matrix 
## cacheSolve is a function that... 

## makeCacheMatrix creates a special matrix "x" that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        ## "i" is the inverse of the matrix 
        i <- NULL
        ## set the value of x 
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        ## gets the value of the vector 
        get <- function () x 
        ## sets the value of i to the inverse of the matrix - (don't have to indicate a variable??)
        setinverse <- function(solve) 
                i <<- solve  
        ## gets the value of i 
        getinverse <- function() 
                i 
        ## returns a list/vector of functions 
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above.  
## If the inverse has already been calculated, then the cacheSolve function retrieves the inverse from the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## checks to see if i has already been calculated
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if i has not already been calculated, then creates a new matrix
        data <- x$get()
        ## then calculates the inverse matrix and sets to "i"
        i <- solve(data, ...)
        ## then sets the cache value of i 
        x$setinverse(i)
        ## returns "i" 
        i
        
}
