## makeCacheMatrix takes a matrix as an arg, and sets or gets the inverse of
## that matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }

  get <- function() x
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  ##list to return of functions
  list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## cacheSolve will cache the results of the inversion for a given matrix

cacheSolve <- function(x, ...) {

   ##get the inverse, check if in cache
   m <- x$getInverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   
   ## set the inverse of the matrix
   m <- solve(data, ...)
   x$setInverse(m)
  ## Return a matrix that is the inverse of 'x'
   m
  
}
