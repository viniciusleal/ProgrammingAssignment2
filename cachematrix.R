## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function make a special Matrix
## Set the values for the matrix and get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the special Matrix
## Retrieve the reverse cache if the reverse has not already been calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if (!is.null(i)) {
           message("getting cached data")
            return(i)
      }
      mat <- x$get()
      i <- solve(mat, ...)
      x$setInverse(i)
      i
}
