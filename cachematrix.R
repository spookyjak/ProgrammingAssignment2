## Put comments here that give an overall description of what your
## functions do
## Return the inverse of a supplied matrix, calculating only when neccessary.

## Write a short comment describing this function
## Sets/gets the value of the supplied matrix
## Sets/gets the inverse value of the supplied matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## Returns the inverse of a matrix created with the makeCacheMatrix
## function (really a list). If the inverse has been calculated through a prior calling of cacheSolve
## then it will look the value up in the cache (the list), otherwise it will calculate it using solve.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
}
