
## makeCacheMatrix: creates "matrix" object that can cache its inverse. return a list of 4 functions 1. sets a matrix 2. geting the value of the matrix 3. setting the inverse 4. geting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


##  computes the inverse of the special matrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
