## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) ## x is the given invertible matrix
{
  ## initializing the inverse of the matrix for the cache
  inv_mat <- NULL 
  set <- function(y)
  {          ##doing this for fun 
                      ##doing this for fun again
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x ## getting the value of matrix
  setinv <- function(x) inv_mat <<- solve ## setting the value of the inverse of matrix
  getinv <- function() inv_mat ## fetching the value of that inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}
