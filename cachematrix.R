## The functions contained in this file compute the inverse of a square matrix
## and stores it in a cache for later use to remove the need to recompute the
## same matrix again.


## This function takes a matrix as an input and outputs a list containing four
## functions that allow the computed inverse and the original matrix to be 
## stored for later use.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Stores the matrix in cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Returns the stored matrix
  get <- function() x
  ## Stores the inverse matrix in cache
  setInvMat <- function(InvMat) m <<- InvMat
  ## Returns the stored inverse matrix
  getInvMat <- function() m
  ## Creates a list containing the 4 functions
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## The cacheSolve function takes the list created in the makeCacheMatrix
## function, searches for a cached inverse and returns if present, 
## otherwise computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
    ## Looks for a cached inverse matrix 
    m <- x$getInvMat()
    ## If cached inverse exists returns stored inverse
    if(!is.null(m)) {
      message("Getting cached data")
      return(m)
    }
    ## Else loads stored matrix, computes inverse and stores in cache
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMat(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
