#the following step completes making a vector with matrices 
# this matrix is a 2 x 2 dimension
a <- makeCacheMatrix( matrix(c(3,7,11,25), nrow = 2, ncol = 2) ) {
  # the following holds the cached value or "NULL"if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  # store a matrix
  setMatrix <- function(newValue) {
    w <<- newValue
    # since the matrix is assigned to a new value, 
    # we push out the cache
    cache <<- NULL
  }
  # the following returns our stored matrix
  getMatrix <- function() {
    w
  }
  # cache is provided in the argument
  # we have a fucntion which deals with cache
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  # the following helps us retrive a cache value
  getInverse <- function() {
    cache
  }
  # This returns a list. 
  #Each of the following elements of the list are a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(v, ...) {
  # get the cached value
  inverse <- v$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- v$getMatrix()
  inverse <- solve(data)
  v$cacheInverse(inverse)
  # return the inverse
  inverse
}
# the following function displays a summary or matrix a
summary(a)
#the $ symbol places and organizes the vectors into a matrix
a$getMatrix()
# cacheSolve function will help compute the inverse of the matrix
cacheSolve(a)
# cacheSolve function will help compute the inverse of the matrix
cacheSolve(a)
