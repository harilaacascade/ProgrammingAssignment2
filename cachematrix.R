## Compute the inverse of a matrix

## makeCacheMatrix : This fucntion provides the cache functions 
## for saving the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getinverse <- function(x) i
  setinverse <- function(inverse) { i <<- inverse }
  list(get = get, set = set, 
       getinverse = getinverse,
       setinverse = setinverse)

}


## cacheSolve : This function returns a cached value of the inverse of the input
## matrix, if it is available. Otherwise, it will calculate the 
## inverse, save it in the cache and return the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("got the inverse from cache")
    return (i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
