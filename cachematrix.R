## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inverse <- x$getInv()
  
  if (!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  
  matrix <- x$get()
  
  Inverse <- solve(matrix, ...)
  
  x$setInv(Inverse)
  
  Inverse
  
}
