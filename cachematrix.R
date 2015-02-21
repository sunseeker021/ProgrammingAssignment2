## These functions will calculate the inverse of a matrix. If the matrix has been cached before it will return the inverse of matrix instantly.

## function makeCacheMatrix creates a special “matrix”, which is really a list containing a function to set and get the value of matrix
## as well as set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of matrix created by makeCacheMatrix. If the inverse of the matrix has already been calculated it
## will get the inverse from cache and will not do any computations. If not it will calculate the inverse and se the inverse matrix in the cache via setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
