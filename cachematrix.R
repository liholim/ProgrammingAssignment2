## For Week 3 of R Programming
## Two functions. The first makes a special 'matrix' object that can 
## cache its inverse. The second computes the inverse of the special
## 'matrix' object or returns the inverse if its been cached.


## Creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse_mat) m <<- inverse_mat
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Given a special 'matrix' object, either computes the inverse
## or returns the cached inverse

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
