## The first function, makeVector creates a list containing
## functions to set/get the matrix and to set/get her inverse


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(Inv) m <<- Inv
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function calculate the inverse of the 'x' matrix
## But first it check if it has already been calculated

cacheSolve <- function(x, ...) {

      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInv(m)
      m
}
