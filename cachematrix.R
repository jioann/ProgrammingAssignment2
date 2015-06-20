## Below are two functions that are used to create a special 
## object that stores a matrix and cache's its inverse.
#
# In function "makeCacheMatrix" the <<- operator is used to assign a value to an object in an environment 
# that is different from the current environment.
# This function creates a special "vector", which is really a list containing functions to
# 1.  set the value of the matrix
# 2.	get the value of the matrix
# 3.	set the value of the inverse
# 4.	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }
# In function "cacheSolve" 
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
# value of the mean in the cache via the setinverse function.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
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

## Test execution
## matrix1 <- matrix(c(1,0,3,2,2,4,3,2,1), ncol=3)
## 
## m1 <- makeCacheMatrix(matrix1)
## m1$get()
##
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    2    2
## [3,]    3    4    1
##
## > cacheSolve(m1)
## [,1]       [,2]       [,3]
## [1,]  0.5 -0.8333333  0.1666667
## [2,] -0.5  0.6666667  0.1666667
## [3,]  0.5 -0.1666667 -0.1666667
## > cacheSolve(m1)
## getting cached data
## [,1]       [,2]       [,3]
## [1,]  0.5 -0.8333333  0.1666667
## [2,] -0.5  0.6666667  0.1666667
## [3,]  0.5 -0.1666667 -0.1666667
##
## first run of cacheSolve saves to cache invers of matrix m1
## second run of cacheSolve retrieves mathematical inverse matrix m1 from cache