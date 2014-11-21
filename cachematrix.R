##############################################
##  Cache inverse matrix computations
##                   ,by Isaias 2014
##############################################
#-This code defines a makeCacheMatrix object.
#  This object implements function calls to get and set a matrix 
#  and the corresponding inverse matrix inside the object.
# Additionaly, an exterinal function cacheSolve, 
#  can re-compute the inverse matrix in makeCacheMatrix.
#  The computation will take place only if necessary, 
#  for instance, after a  $set(...) call.

##    makeCacheMatrix:
# Initializes a special "matrix" object that can store a matrix and its inverse.
# SYNTAX: newmatrix <- makeCacheMatrix(...) # Initialize object, store matrix and compute the inverse.
#         newmatrix$get()      # Returns stored matrix
#         newmatrix$set(...)   # Set matrix to ...
#         newmatrix$getinv()   # Returns the inverse of the stored matrix
#         newmatrix$setinv(...) # Manually sets the inverse of the stored matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL        # Initializes to empty the inverse matrix
  set <- function(y) { 
    x <<- y          # Reset matrix: Use <<- to access matrix "x" in parent environment
    m <<- NULL       # Each time we reset the matrix, empty the cached inverse
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(myinv) inv <<- myinv # the inv matrix in object environment
  # Returned object: List of functions with an 
  #   Each function has associated an environment
  #   in which x and inv are permantly stoted.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##    cacheSolve:
# Compute the inverse of the matrix stored in a makeCacheMatrix ONLY if necessary.
# SYNTAX: makeCacheMatrix(...) 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()  # Get cached inverse matrix from the supplied object's environment
  ## Return a matrix that is the inverse of 'x':
  if(!is.null(inv))   return(inv) # If inv is present, return the same (no extra computation)
  inv <-  solve( x$get() ) #  Else: get matrix, compute
  x$setinv( inv )          # Store into objec
  return(inv) 
}

### Example:
# New object with a 3x3 random-valued matrix
newmatrix <- makeCacheMatrix( matrix(runif(90000),300,300) ) 
# Check inverse is empty:
newmatrix$getinv() # Returns NULL
# 1st time: Computation time of the inverse matrix:
system.time(cacheSolve(newmatrix)) # 0.061 seconds ellapsed
# 1nd time: Computation time of the inverse matrix:
system.time(cacheSolve(newmatrix)) # Zero seconds ellapsed
