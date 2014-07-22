# A straightforward implementation of a "caching matrix", i.e.
# a storage for a matrix which also caches its inverse and computes it
# upon request.
#
# This code follows the Google R Style Guide:
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
# In particular, the indentation is 2 spaces per level, in contrast
# with what is used in the lectures.


makeCacheMatrix <- function(x=matrix()) {
  # Makes a new "caching matrix".
  #
  # Args:
  #   x: initial value of the matrix
  #
  # Returns:
  #   A "caching matrix".
  m <- x  # unless this is done, get() may return a circular reference
  inv <- NULL
  get <- function() m
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


cacheSolve <- function(x, ...) {
  # Computes the inverse of a "caching matrix", either by retrieving
  # from cache or from scratch.
  #
  # Args:
  #   x: a "caching matrix"
  #
  # Returns:
  #   The inverse of x.
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)
  }
  inverse <- solve(x$get())
  x$setinv(inverse)
  inverse
}
