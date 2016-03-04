## These functions implement cached matrix data structure. 

## This function creates a data structure that stores pointers to cached matrix 
## Input - a regular matrix to to be cached 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solve matrix stored in cached matrix data structure.
## The first call to function will calculate inverse 
## matrix and all subsequent calls will return already
## calculated result
## Input - cached matrix created by makeCacheMatrix() function
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
