## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create functions to set and get matrix
## and setinverse and get inverse of same matrix which are saved as list of 4 elements
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) x_inverse <<- solve
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function first checks if there is no inverse matrix in cache
## if there is none it calculates inverse of matrix and saves it in cache
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
