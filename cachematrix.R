## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a cached Matrix.
## It actually a list having set, get of matrix and get set of inverse cached
makeCacheMatrix <- function(x = matrix()) {
  
  #store inv
  cachedInv <- NULL
  
  #Set the matrix value
  set <- function(y) {
    x <<- y
    cachedInv <<-NULL
  }
  
  #Get the matrix 
  get <- function() x
  
  setInverse <- function(inverseMatrix) cachedInv <<- inverseMatrix
  getInverse <- function() cachedInv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


## Write a short comment describing this function
# get the inverse from the cacheMatrix, if not cached, calculate and store.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 
  invX <- x$getInverse()
  
  if (!is.null(invX)) {
    message("getting cached data")
    return(invX)
  } else {
    message("calculate inverse")
  }
  data <- x$get()
  invX <- solve(data)
  x$setInverse(invX)
  invX
}
