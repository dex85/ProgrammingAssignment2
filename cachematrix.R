## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a specific matrix and input. 
## The function returns a special matrix with inegrated functions 
## and cached values for already calculated inputs. In detail it
## caches the invert of a matrix. It is assumed that all inputs
## are always invertible. Given that, it is essentially a helper
## function for the following cachesolve function.

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  
  set_mtrx <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  
  get_mtrx <- function() x
  set_inverse <- function(solve) mtrx <<- solve
  get_inverse <- function() mtrx
  list(set_mtrx = set_mtrx, get_mtrx = get_mtrx,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function calculates the invert of the before mentioned special matrix.
## The difference to the usual way is, that it checks or looks up for cached values
## and sets them if available in order to save computing time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtrx <- x$get_inverse()
  if(!is.null(mtrx)) {
    message("access cached values")
    return(mtrx)
  }
  data <- x$get_mtrx()
  mtrx <- solve(data, ...)
  x$set_inverse(mtrx)
  mtrx
}
