## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##assign NULL to inv as placeholder for future
  
  ## function set sets the value in a parent 
  ## environment(makeCacheMatrix scope) that contains a variable called x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## function get returns variable x in makeCacheMatrix scope 
  get <- function() x
  
  ## function setinv sets the value in a parent 
  ## environment(makeCacheMatrix scope) that contains a variable called inv
  setinv <- function(m_inv) inv <<- m_inv
  
  ## function getinv returns variable inv in makeCacheMatrix scope
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## try to return cached inv  
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##if no cached inv, call solve function to calculate inv
  ## and cache inv for future use
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(matrix)
  inv
  
}
