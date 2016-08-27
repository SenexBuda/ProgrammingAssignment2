## A pair of functions to enable caching of matrix inverse computations,
## which can be time consuming for large matrices

## makeCacheMatrix takes an ordinary R matrix and creates 
## a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function() inv
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed (i.e. not set to a new matrix), 
## then cacheSolve retrieves the inverse from the cache.
## The matrix argument is assumed to be invertible. If not an error will be thrown.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
