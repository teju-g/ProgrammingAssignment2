## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  setmtrx <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  getmtrx <- function() x                                 
  setinverse<- function(inverse) inv_x <<-inverse     
  getinverse <- function() inv_x						
  list(setmtrx = setmtrx, getmtrx = getmtrx,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()                            
  if (!is.null(inv_x)) {
    return(inv_x)
  } else {
    inv_x <- solve(x$getmtrx())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
