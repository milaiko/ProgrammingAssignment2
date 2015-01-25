## Put comments here that give an overall description of what your
## functions do

## --------------------------------------------------------------------------------
## This function creates a special matrix object that 
## 1. provides a setter of the matrix
## 2. provides a getter of the matrix
## 3. provides function setinverse() to assign computed inverse matrix
## 4. Provides function getinverse() to obtain the cached inverse matrix
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## --------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated,
## then the function retrieves the inverse from the cache.
## If not (esle) it computes the inverse and sets the value in the cache.
## --------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  else{
    message("No cached data found.")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
}
