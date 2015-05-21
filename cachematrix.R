## The following script contains 2 functions. The first one creates a R matrix object containing
## functions to get and set a matrix of numbers and get and set its inverse
## the second one looks on the object returned by the first one and sees if the inverse is calculated.
## If so, it returns its value, otherwise it calculates it.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  matrix(list(set, get,
      setinverse,
      getinverse),2,2)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of the one contained in 'x'
  i <- x[[2,2]]() # the getinverse() function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x[[2,1]]()  # the get function
  i <- solve(data,...)  # computes the inverse
  x[[1,1]](i)   # sets the inverse in the object
  i   #returns the inverse
}
