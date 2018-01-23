## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## @param x the matrix that will be inversed and cached.

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL                             
    set <- function(y) {                     
      x <<- y                             
      inve <<- NULL                        
    }
    get <- function() x                    
    setinve <- function(inverse) inve <<- inverse  
    getinve <- function() inve 
    list(set = set, get = get, setinve = setinve, getinve = getinve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinve(inve)
  inve
}
