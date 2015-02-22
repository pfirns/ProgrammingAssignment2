## Put comments here that give an overall description of what your
## functions do
##
## The 'makeCacheMatrix' function creates global variables and assigns initial values to these variables. 
## It then creates a collection of functions that can be used to maniulate the global variables
## including reassigning values to them.
##
## The 'cacheSolve' function retrieves values of the global variables and uses functions created in the 
## 'makeCacheMatrix' function to manipulate and recalculate values for global variables.
##
## Together, the two functions demonstrate how to use the Lexical scoping characteristic of R to 
## cache calculated values s that they can be re-used.
##

## Write a short comment describing this function: 
#  Function makeCacheMatrix: 
#
#  Argument: a numeric matrix
#  Returns: a list containing a collection of functions
# 
#  This function creates the following:
#     1. Cacheable matrices x and inv 
#          (assigning values to global variables means the values become available in the 
#           environment in which the function was defined - effectively caching the variables
#           and making the values available to other functions defined within the same environment - 
#           in this case the global environment)
#
#     2. A list of functions to manipulate the cached matrices 
#          (these functions can be invoked from within the environment in which this function is defined)
#
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- matrix(NA_real_)
  
  ## set function reinitialises matrices and assigns the new values to global variables x and inv : 
  ##            argument y is assigned to global variable x; 
  ##            matrix initialised to NA is assigned to global variable inv  
  set <- function(y = matrix()) {
    x <<- y                 
    inv <<- matrix(NA_real_) 
  }
  
  ## get function returns the value of global variable x
  get <- function() x
  
  ## setinv function to cache the inverted matrix (ie. assigns the value to global variable)
  setinv <- function(solve) inv <<- solve
  
  ## getinv function returns whatever value is currently assigned to global variable inv
  getinv <- function() inv
  
  ## returns a list of functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#  Function cacheSolve:
#
#  Argument: the list of functions defined in the 'makeCacheMatrix' function above.
#  Returns: A matrix that is the inverse of 'x'
#           (x is a global variable defined in the 'makeCacheMatrix' function above. 
#            New values can be assigned to x either by invoking the 'makeCacheMatrix' function 
#            or by invoking the 'set' function which is one of the functions returned by 
#            'makeCacheMatrix')
#
#  Description:
#  This function first tests to see whether there is a cached value for the global variable that holds the
#  inverted matrix.
#  If there is a cached value, the function returns that value with a message.
#  If there is no cached value, the function retrieves the original matrix, x,
#  calculates the inverse and returns the value from row 1, column 1.
#
#  The function uses the following functions which are included in the argument list:
#   getinv
#   get
#   setinv
#
cacheSolve <- function(x, ...) {
  
  # invoke 'getinv' function to assign value of global variable 'inv' to local variable 'inverse'
  inverse <- x$getinv()
  
  # if value in row 1, column 1 is not "NA", then the inverse will not be re-calculated 
  # ie. the cached value will be returned along with a message stating this.
  if(!is.na(inverse[1,1])) {
    message("Getting cached data")
    return (inverse)
  }
  
  # invoke 'get' function to retrieve current value of global variable 'x'
  data <- x$get()
  
  # Calculate inverse of matrix and assign to local variable 'inverse'
  inverse <- solve(data)
  
  # invoke 'setinv' function to assign value of 'inverse' to global variable 'inv'
  x$setinv(inverse)
  
  # return inverted matrix
  inverse
}
