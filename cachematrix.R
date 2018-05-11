## Functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  n <- NULL
  set <- function(y) {
    
    x <<- y
    n <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The second function consider the inverse of the "matrix" created 
## above. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          
  n <- x$getInverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  mat <- x$get()
  n <- solve(mat, ...)
  x$setInverse(n)
  n
}