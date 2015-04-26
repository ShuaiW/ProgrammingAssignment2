## Matrix inversion is usually a costly computation and there are some benefits
## to cache the inverse of a matrix rather than computing it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a
## matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL                                    # inverse = null
    
    set <- function(mtrx) {                        # matrix setter
        x <<- mtrx
        inv <- NULL
    }
    
    get <- function() x                            # matrix getter
    
    setinv <- function(inverse) inv <<- inverse    # inverse setter
    
    getinv <- function() inv                       # inverse getter
    
    list(set = set, get = get,                     # functions registration
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()                              # get inverse
    
    if(!is.null(inv)) {                            # cached
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()                                # compute and set inverse
    inv <- solve(data)
    x$setinv(inv)
    return(inv)     
}


