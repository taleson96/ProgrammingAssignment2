## File: cacheMatrix.R
## Description:
##   This file contains the following functions:
##
##    * makeCacheMatrix(x) - returns a 4 function list for matrix x. 
##          - Note:  functions are set,get,setinv,getinv.
##
##    * cacheSolve(x,...) - gets cached inverse of matrix x, otherwise it will calculate and cache inverse.
##          - Note:  additional arguments are fed to solve().
##          - Note2: x is a makeCacheMatrix() "object"


##------------------------------------------------------
## - makeCacheMatrix(x)
##    - This function takes in a matrix input and creates a special vector to do 4 things:
##      1. set - set value of matrix
##      2. get - get value of matrix
##      3. setinv - set value of inverse matrix. (assuming always invertible)
##      4. getinv - get value of inverse matrix.
##------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  # Initialize value of saved inverse.
  xinv <- NULL
  
  #use lexical scoping to set x using this function.
  set <- function(y) {
    # Set x and clear the now invalid inverse.
    x    <<- y
    xinv <<- NULL   
  }
  
  #Generic Function to 
  get    <- function() x
  
  #Save the inverted matrix value.
  setinv <- function(pinv) xinv <<- pinv
  getinv <- function() xinv
  
  #return list of functions
  list(  set=set , get=get , setinv=setinv , getinv=getinv)
  
}


## Write a short comment describing this function
##------------------------------------------------------
## -cacheSolve(x,...)
##    - This function returns and caches the inverted value of the matrix "x"
##    - if the value is already cached, then return the cached value.
##    - It will also pass any arguments to the solve function.
##------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Grab the cached value..
  xinv <- x$getinv()
  
  # If it exists, the return it,
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  #"Else" if it doesn't, then calculate it and save it.
  data <- x$get()            # Get the x matrix.
  xinv <- solve(data, ...)   # calculate the inverse
  x$setinv(xinv)             # cache the inverse
  xinv                       # return inverse.
}
