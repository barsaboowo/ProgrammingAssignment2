## Functions for creating and utilising a matrix object that contains a cached inverse

## Returns a matrix object with internal cache of its inverse (not set on creation)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a matrix object with cached inverse, returns cached inverse if already set.  Otherwise, calculates inverse, 
## caches it and returns the inverse.  Matrix supplied must be a square matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("Returning the cached inverse")
          return(i)          
        }
        data <-x$get()
        i <-solve(data)
        x$setinverse(i)
        i        
}
