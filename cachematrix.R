#---------------------------------------------------------------------------
# The following 2 functions implement a scheme to compute the inverse of a
# matrix and caches the result. Subsequent calls to invert the matrix will
# return the inverse from the cache rather than re-computing the inverse
# again
#
# The scheme consists of 2 functions:
# 1. makeCacheMatrix() - create and store a 'special' form of the matrix 
# 2. cacheSolve() - invert the 'special' matrix and cache the result
#---------------------------------------------------------------------------

# makeCacheMatrix()  - this is passed a matrix. It 'stores' the matrix in the
#                      function environment and returns a list of 4 functions
#                      for working on the 'stored' matrix:
#                      1. set the value of the matrix
#                      2. get the value of the matrix
#                      3. set the value of the inverse
#                      4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                           # inverse holds the inverse of the input matrix  
  set <- function() {                       # set() resets the stored object
    x <<- y
    inverse <<- NULL
  }
  get <- function() {x}                     # get() returns the stored object
  setInverse <- function(i) {inverse <<- i} # setInverse() sets object's inverse
  getInverse <- function() {inverse}        # getInverse() returns the object's inverse
  # return the 4 functions - set(), get(), setInverse() and getInverse() in a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve() - this is passed a 'special matrix' (as returned from
#                makeCacheMatrix()) and returns the inverse. The first time
#                the function is called it will call solve() to compute the
#                inverse and then cache the inverse. Subsequent calls to the
#                function will return the inverse from the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()                 # obtain the stored object's inverse
  if (!is.null(inverse)) {                  # Q. first call ?
    message("returning inverse from cache") # A. no -  return from cache
    return(inverse)
  }
  mat <- x$get()                            # first call so retrieve the stored object
  inverse <- solve(mat, ...)                # ... compute it's inverse
  x$setInverse(inverse)                     # ... and store in cache
  inverse                                   # return the inverse 
}
