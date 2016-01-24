## There are two functions needed for this assignment 
## The first will create a special matrix and store it into cache
## The second will calculate the inverse of the special matrix.  If the inverse was already calculated 
## it will return it from storage (cache)
## 


## This function creates the special matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## see if you already have the inverse of the matrix in cache, if yes use it, if no calculate it
  if(!is.null(m)) {
    message("Using cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## output the inverse
  m
}