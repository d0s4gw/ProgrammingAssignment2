## makeCacheMatrix builds an object that contains a matrix
##  and a cache value for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## cacheSolve takes in the object built by makeCacheMatrix
##  and returns the inverse of the matrix.  If the cacheMatrix
##  object does not already have the inverse matrix cached
##  then cacheSolve will compute the inverse and then store it
##  in the cacheMatrix object
cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmatrix(m)
  m
}
