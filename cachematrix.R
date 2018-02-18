## The following 2 functions caches the inverse of a matrix, when given a invertible matrix. 

## makeCacheMatrix: creates a list with 4 elements, and assigns a function to each. Run the below to see details of elements.
## > a <- makeCacheMatrix()
## > a
##  set = set the value of the matrix
##  get = get the value of the matrix
##  setinv =  the value of inverse
##  getinv =  the value of inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(sinv) m <<- sinv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: solves the inverse of matrix returned by makeCacheMatrix ('matrixname'$get)
## it will first check if the inverse is already calculated, if so it will return the inverse from the cache, if not (m is null) it will 
## compute the inverse, and it will be cached 
cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
## Sample Run
## > source("cachematrix.R")
## > b = matrix(c(1,2,-2,0,3,5,-6,3,5), nrow = 3, ncol = 3) 
## > matr <- makeCacheMatrix(b)
##
## > cacheSolve(matr)
##[,1]       [,2]     [,3]
##[1,]  0.0000000 0.31250000 -0.18750
##[2,]  0.1666667 0.07291667  0.15625
##[3,] -0.1666667 0.05208333 -0.03125
##
##> cacheSolve(matr)
##getting cached data
##[,1]       [,2]     [,3]
##[1,]  0.0000000 0.31250000 -0.18750
##[2,]  0.1666667 0.07291667  0.15625
##[3,] -0.1666667 0.05208333 -0.03125
##
##
