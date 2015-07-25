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
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## The following function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse had already been calculated(and the matrix has not changed),then the cacheSolve will
## retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
