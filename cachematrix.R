## These functions allow caching the inverse of a matrix to avoid
## unnecessary computations

## makeCacheMatrix returns a list of functions:
## 1. set - Sets value of the matrix
## 2. get - Gets value of the matrix
## 3. setInverse - Sets inverse of the matrix
## 4. getInverse - Gets inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  } 
  
  setInverse <- function(mInverse){
    inverse <<- mInverse
  } 
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix, and saves it to cache. 
## If the inverse has already been calculated
## and the matrix has not changed, the value from cache is returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  
  inverse <- solve(matrix, ...)
  
  x$setInverse(inverse)
  
  inverse
}
