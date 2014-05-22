# The object of these functions together is to cache the inverse of a matrix
# which is created by inputting it into the first function. 
# Whenever the  function makeCacheMatrix  is called it sets the inverse to NULL, 
# so that the second function, cacheSolve, 
# knows that it has to recalculate the inverse.
# Otherwise, when the second function is called on a matrix that was created 
# by makeCacheMatrix, it returns the cached inverse. 
# On the other hand, if the first function was called again in the meanwhile,
# to recreate this same matrix, it recalculates the inverse.

## This first function creates the functions which will define the matrix, 
# when the second function is called, and also sets the inverse to NULL to
# indicate that the inverse has to be recalculated since this is a new matrix. 

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv<<-inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function does all the work. It first checks to see whether the 
# inverse of the labeled matrix already exists. If so, it returns the 
# cached inverse. If not, it solves for the inverse, sets this to be the 
# inverse of the matrix (so it is not NULL anymore), and returns this inverse.


cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
