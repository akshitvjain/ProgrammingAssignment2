## Put comments here that give an overall description of what your
## functions do

## This function created list of function, which imitates object-like behaviour.
## We may perform four actions:
## set - save matrix in our 'object' and clear inverse matrix value
## get - retrive matrix
## setinv - set value of inv variable (supposed to be inverse matrix)
## getinv - retrive value of inv variable (supposed to be inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinv <- function(invMatrix) inv <<- invMatrix
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## This function calculates inverse matrix using 'solve' function with default 'b' argument
## to save the time result is saved using the scope rules of R. Next time it uses saved result
## function works only with 'objects' created with makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
