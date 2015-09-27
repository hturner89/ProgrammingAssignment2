# The function makeCacheMatrix creates a list containing 
# a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function(inverse) m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The following function calculates the inverse of the matrix created by the 
# above function. However, it first checks if the inverse has already been 
# calculated. If so, it retrieves the result from the cache and skips the
# calculation. Otherwise, it calculates the inverse of the matrix, and sets
# the value in the cache using setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
  }

