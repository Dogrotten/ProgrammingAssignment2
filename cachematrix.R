## This functions save the inverse of a matrix to be used in multiple calculations 
## avoiding the need to calculate the inverse each time unless the matrix changes.
## The procedure is: Define your matrix, use it in makeCacheMatrix to assemble the
## functions list, then calculate the inverse using  cacheSolve.

## makeCacheMatrix assembles a list of four functions as described next:
## set: is used to change the matrix values and the inverse needs to be calculated again.
## get: stores the matrix.
## setinv: changes the inverse for the newly calculated.
## getinv:stores the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invm <- matrix() 
  set <- function(y) { 
    x <<- y 
    invm <<- matrix() 
  }
  get <- function() x 
  setinv <- function(inv) invm <<- inv 
  getinv <- function() invm 
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if the inverse is available.
## If it does displays a message and returns its value.
## If not then gets the matrix, calculates the inverse, stores it and returns its value.
cacheSolve <- function(x, ...) {
  invm <- x$getinv() 
  if(!all(is.na(invm))) { 
    message("getting cached matrix") 
    return(invm)
  }
  matrix <- x$get()  
  invm <- solve(matrix, ...) 
  x$setinv(invm) 
  invm 
}
