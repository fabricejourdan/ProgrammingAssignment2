##Put comments here that give an overall description of what your functions do

##Those functions were created in order to use cache for time-consuming computations like Matrix inversion.
##In our case, calculate the inverse of a matrix is typically a fast operation. 
##However, for a very large matrix, it may take too long to compute the inverse of the matrix, 
##especially if it has to be computed repeatedly (e.g. in a loop). 
##If the contents of a inversed matrix are not changing, it may make sense to cache the 
##value of the inversed matrix so that when we need it again, it can be looked up in the
##cache rather than recomputed

##Write a short comment describing this function

##The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inversed matrix
##get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function () m
  list(set = set, 
       get= get, 
       setinversematrix = setinversematrix, 
       getinversematrix, getinversematrix)
}

##Write a short comment describing this function

##The following function calculates the inverse of the special "matrix" created with the 
##above function. However, it first checks to see if inversed matrix has already been
##calculated. If so, it gets the inversed matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inversed 
##matrix in the cache via the setinversedmatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if (!is.null(m)) {
    message("getting cached inversed matrix")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversematrix()
  m  
}
