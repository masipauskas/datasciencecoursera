# makeCacheMatrix - create a cached matrix inverse object for a given matrix:
#   m - matrix, to create a cached inverse object for
# returns - cached matrix inverse object m:
#   m$get() - gets the matrix
#   m$set() - sets the new matrix and resets inverse
#   m$getInverse() - returns a cached version of inverse, or NULL if none cached
#   m$setInverse() - sets a nev version of inverse to cache
makeCacheMatrix <- function(m = matrix()) {
  cachedInverse <- NULL
  
  # if matrix is different from previous, then
  # sets a new value to a matrix, resets the cached inverse (as matrix is now new).
  set <- function(y) {
    if (!identical(m,y)) {
      m <<- y
      cachedInverse <<- NULL
    }
  }
  
  # returns original matrix.
  get <- function() m
  
  # setter to cache result of an inverse.
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  # getter to retrieve cached inverse.
  getInverse <- function() cachedInverse
  
  # returns a new vector / object, with getter and setter for matrix
  # and its cached inverse.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# returns a matrix, which is inverse of x
# x - cache matrix holder object, created using makeCacheMatrix
# returns - cached inverse if available, otherwise calculated inverse
cacheSolve <- function(x, ...) {
  # check if cached data is available
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  
  # get the original matrix, calculate the inverse and cache the result
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  
  #return calculated inverse
  inverse
}