## these functions return a cached inverse of an invertible matrix that is preprocessed

# makeCacheMatrix preprocesses an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the function set : sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  
# the function get : gets the value of the matrix
  get <- function(){ x} # get value of x
  
  # the function setSolve : sets the value of the inverse matrix
  setSolve <- function(solve) m <<- solve # caching the inverse matrix of x
  
  # the function getSolve : gets the cached value of the inverse matrix
  getSolve <- function() m # getting the cached inverse matrix of x
  
  # returning the results 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

# function cacheSolve : returns the cached inverse of a matrix
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

# example 
x <- makeCacheMatrix(matrix(1:4,2))
x
cacheSolve(x)


