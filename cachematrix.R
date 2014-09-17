## This R script is done for the Programming Assignment 2 for R Programming on Coursera

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #initially the inverse matrix cache value is set as NULL
  invMatrix <- NULL             
  
  #set function within the main function is used to define matrix value.
  #it also clears cached value of the inverse matrix every time a new matrix is defined
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  #get function returns the matrix value
  get <- function() x
  
  #setinv function is used to update cached value of the inverse matrix
  setinv <- function(inverseMatrix) invMatrix <<- inverseMatrix
  
  #getinv function returns cached value of the inverse matrix
  getinv <- function() invMatrix
  
  # if no arguments are given the function will return a list containing each of the subfunction
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve function is used to check whether a cached value of the inverse matrix is stored
## and to either return the cached value, or calculate and store a new cache value if it doesn't exist.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinv()
  
  ## check whether cached value exist and return that
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  ## if cached value is not stored, 
  ## 1) calculate inverse matrix value,  
  ## 2) store that to the variable and 
  ## 3) return the inverse matrix value
  
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setinv(invMatrix)
  invMatrix
  
}
