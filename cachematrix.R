## Caches matrix values in order to save time

## Contains functions for setting and getting the value and inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(value) inv <<- value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of a matrix if no previous value exists
## If one does exists, takes out that value

cacheSolve <- function(x, ...) {
  ## Checks if value already exists
  if (!is.null(x$getinv())){
    message("getting cached data")
    x$getinv()
  }
  else{
    x$setinv(solve(x$get(),...))
    x$getinv()
  }
  
}
