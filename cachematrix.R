## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.


## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  # it gets input matrix as x
  m <- NULL  # initialize a temp variable
  set <- function(y) {  # implement set operation
    x <<- y
    m <<- NULL
  }
  get <- function() x   # get operation to receive the input matrix
  setinverse <- function(inverse) m <<- inverse # set the inverted matrix
  getinverse <- function() m   # get inverse operation
  list(set = set, get = get,   # makeCacheMatrix returns a list of
       setinverse = setinverse,  # the 4 operations
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  # initialize a temp variable
  if(!is.null(m)) {    # check if inversion has already been performed
    message("getting cached data")
    return(m)          # if already performed, just return the stored inversion
  }
  data <- x$get()      # if not already performed, get the matrix and store it to a temp variable
  m <- solve(data, ...)  # perform the matrix inversion
  x$setinverse(m)
  m                    # return the inverted matrix
}