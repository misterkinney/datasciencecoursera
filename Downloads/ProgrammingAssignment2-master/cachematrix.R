## Funtion to cache Matrix Inversion for Assigment 2 Coursera Data Science class
## Michael Kinney $ misterkinney@yahoo.com

## Function for invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    ## return: a list containing functions to
    ##              1. set the value of the matrix
    ##              2. get the value of the matrix
    ##              3. set the value of the inverse
    ##              4. get the value of the inverse
    ##         this list is used as the input to cacheSolve()
    
    inv = NULL
    set <- function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## x is the output of makeCacheMatrix()
    ## return: inverse of makeCacheMatrix()
    
    inv = x$getinv()

    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}
