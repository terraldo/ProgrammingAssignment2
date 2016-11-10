## The following functions work together to create a special invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix:This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("fetching cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
