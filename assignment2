##########################################################

##creates a "matrix" and its inverse:
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

##computes the inverse of matrix:
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {  # checks if the inverse has already been calculated
      message("getting cached data...")
      return(m) #returns the inverse from the 'makeCache'
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
