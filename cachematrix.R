## Put comments here that give an overall description of what your
## functions do

## Objective Specified (1)
##`makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 1.  set the value of the matrix   (from example)
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL

  ## 1.  set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  
  }

  ## 2.  get the value of the matrix
  get <- function() { x }
  
  ## 3.  set the value of the inverse  
  setInverse <- function(x) { 
	inv <<- x  
	 
	}

  ## 4.  get the value of the inverse
  getInverse <- function() { inv }

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Objective Specified (2)
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...) ## Solve with 1 parameter gives inverse
    x$setInverse(inv)
    inv
}
