
## The following functions provide a way to cache the inverse of a matrix, so
## it doesn't need to be frequently recalculated.
## To use:
## 1) Create your "matrix" with makeCacheMatrix(<matrix>). Ex:
## > m <- makeCacheMatrix(matrix(1:4,2,2))
## 2) Use cacheSolve on your cacheMatrix the same way you would use solve(). Ex:
## > inverse <- cacheSolve(m)
## cacheSolve() only calculates an inverse once for each cacheMatrix.


## Creates a matrix object that will cache the inverse of a matrix.
## Returns a list of 4 functions:
## setMatrix(x) Registers x as the matrix whose inverse we'll cache.
## getMatrix() Returns x from most recent call to setMatrix()
## setInverse(inv) Saves inv as the value of the inverse of x.
## getInverse() Returns inv from the most recent call to setInverse().
##              Returns null if setInverse() wasn't called after setMatrix()

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  setMatrix <- function(y){
    x <<- y
    i <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverse <- function(inv){
    i <<- inv
  }
  getInverse <- function(){
    i
  }
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,
       getInverse=getInverse)
}


## Returns the inverse of matrix x using the solve() function.
## ... is passed to the solve() function

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)){
    return(i)
  }else{
    x$setInverse(solve(x$getMatrix()))
    return(x$getInverse())
  }
}
