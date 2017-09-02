#this code completes R Programming//Programming Assignment 2

#there are two functions
#makeCacheMatrix creates an empty matrix (x) and creates the necessary functions for cacheSolve
#cacheSolve computes the inverse of the matrix 
#or retrives it from cache if it's already been calculated


##this script also contains a few test matricies

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m 
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}


##here are the matricies I tested with, and their correct inverse
##the already existing inverse variables were used to test accuracy

##for example, once the makeCacheMatrix and cacheSolve functions have been called
##i stored the result of cacheSolve in a variable named inverse1 (for example)
## and then ran inverse1 == i1
##if the matrix returns all true, 
##then the inverse was calculated correctly in the cache function

testmatrix1 <- matrix( 
  c(1,2,3,4), 
  nrow=2, 
  ncol=2)

i1 <- solve(testmatrix1)

testmatrix2 <- matrix( 
  c(24, 4, 31, 1, 5, 76, 6, 7, 8), 
  nrow=3, 
  ncol=3) 

i2 <- solve(testmatrix2)

