# Put comments here that give an overall description of what your

# The overall purpose here to Cache the Inverse of a Matrix:
# Matrix inversion is usually takes lots of memory and caching the inverse of a matrix would be helpful 
# rather than compute it every time.
# The two functions here, store a matrix and cache its inverse


# The makeCacheMatrix function creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The cacheSolve function below calculate the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated, for the same  
# matrix, then it should return the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}