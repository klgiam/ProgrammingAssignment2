## makeCacheMatrix is used to create a special matrix that can cache its inverse.
## cacheSolve is to cache the inverse of the special matrix
## Example of usage: 
## 1) m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2) ## create a 2 by 2 matrix
## 2) m2 <- makeCacheMatrix(m1) ## create the special version of the matrix 
## 3) cacheSolve(m2) ## calculate and cache the inverse of the matrix
## 4) m2$getinverse() ## fetching the cached inverse matrix  


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
