## Put comments here that give an overall description of what your
## functions do

## Below I define the function makeCacheMatrix
## makeCacheMatrix creates a special matrix object that can cache its inverse for the input.
## input must be a square matrix that can be inverted.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {  
    x <<- y  
    inv <<- NULL  
  }  
  get <- function() x  
  setinv <- function(inverse) inv <<- inverse  
  getinv <- function() inv  
  list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Below I define the function cacheSolve
## cacheSolve calculates the inverse of x if this has not already been done. 
## If the inverse has been caluclated already - and the matrix is the same - then
## the function will retreive the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  
  if(!is.null(inv)) {  
    message("getting cached result")  
    return(inv)  
  }  
  data <- x$get()  
  inv <- solve(data, ...)  
  x$setinv(inv)  
  inv
}

## TESTING THE WORK
# This is to test the functions work as expected

# testMatrix <- matrix(rnorm(16),4,4)   
# testCache <- makeCacheMatrix(testMatrix)  
# cacheSolve(testCache)  
