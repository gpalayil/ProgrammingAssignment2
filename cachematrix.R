## Calculating the inverse of a matrix is typically a compute intensive operation. The objective of
## of the following two functions is to  cache the inverse of the matrix so that it can be read from
## the cache and not calculated repeatedly
## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## The second function "cacheSolve" will compute the inverse of the matrix returned by "makeCacheMatrix"
## function.

## The makeCacheMatrix function will create a special matrix object that can cache its inverse.
## The function creates a list containing a functon to:
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of inverse of the Matrix
## 4. get the value of inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminverse <- function(inverse) minv <<- inverse
  getminverse <- function() minv
  list(set=set, get=get, setminverse=setminverse, getminverse=getminverse)
}




## The following function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse had already been calculated(and the matrix has not changed),then the cacheSolve will
## retrive the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  minv <- x$getminverse()
  if(!is.null(minv)) {
    message("getting cached data.")
    return(minv)
  }

  data <- x$get()
  minv <- solve(data)
  x$setminverse(minv)
  minv
}
