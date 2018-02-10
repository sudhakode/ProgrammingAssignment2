
## The pair of functions below will cache the inverse of the matrix that is invertible

## The function makeCacheMatrix will create an object that can cache the inverted matrix.

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

## The function cachesolve will compute the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## Solve function is used to get the inverse of the matrix.
  
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}

## Function calls:
## matrix_orig <- makeCacheMatrix(matrix( c(5, 1, 0,3,-1, 2, 4 , 0 , -1),nrow = 3,byrow = TRUE))
## matrix_orig$get()
## cacheSolve(matrix_orig)
## matrix_orig$getinvmatrix()
## if you run cacheSolve(output) again then it will print getting cached data

            