## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix - creates a matrix variable with cacheble inverser

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function () x
  setinv <- function (invm) I <- invm
  getinv <- function () I
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve - encapsulates the caching functionality of obj from makeCacheMatrix
## Note: library(MASS) needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinv()
  if (!is.null(I)) {
    message("Getting cahced inversed")
    return (I)
  }
  data <- x$get()
  I <- ginv(data,...)
  x$setinv(I)
  I
}
