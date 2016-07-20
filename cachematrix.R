## Those functions allow to cache the inverse of a matrix; 
## by this way, they reduce the computation time for repetitive 
## usage of the value of the inverse.

## This function creates an object that can cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
}


## This function retrieves the inverse of the matrix if already computed, 
## and computes it if not. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
      
}
