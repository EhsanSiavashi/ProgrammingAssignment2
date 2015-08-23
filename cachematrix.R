## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function gets a matrix as its input and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  Inverse <- NULL
  
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inv) Inverse <<- inv
  
  getInv <- function() Inverse
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
## If the matrix is not changed, the inverse of it will be fetched from the cache. Otherwise, it needs to be recalculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inverse <- x$getInv()
  
  if (!is.null(Inverse)) {
    message("getting cached inverse of the matrix")
    return(Inverse)
  }
  
  matrix <- x$get()
  
  Inverse <- solve(matrix, ...)
  
  x$setInv(Inverse)
  
  Inverse
  
}
