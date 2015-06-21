## This pair of functions cache the inverse of a matrix.
## The <<- operator is used in both functions below to assign
## values to objects in an evironment that is different from
## the current environment.

## This first function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  xInv <- NULL  #"xInv" stores the inversion result; initialized here
  ## Next we need a setter function...
  ## This function sets a matrix to an object that was previously created
  ## by the makeCacheMatrix function.
  set <- function(y) {
    x <<- y
    xInv <<- NULL  # New function, so we must re-initialize "xInv".
  }
  
  get <- function() x  # Returns the input matrix.
  setInv <- function(inv) xInv <<- Inv  # Sets the inversed matrix.
  getInv <- function() xInv  # Returns the inversed matrix.
  ## Next, return a list that contains these functions, so that
  ## we can use objects computed in this function elsewhere (like
  ## in the next function).
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This second and final function computes the inverse of the "matrix" returned from above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()  # Retreive the inversed matrix from object "x," defined above.
  ## Remember...it will be null if uncalculated! (defined above)
  if(!is.null(m)) {  # Iff the inversion result is there...
    message("Getting cached data.")
    return(m)  # Return the calculated inversion.
  }
  data <- x$get()  # Iff the inversion result isn't already there, we retreive "x$get" to
                   # obtain the matrix object.
  m <- solve(data)  # Solve for the inverse using R's built-in "solve" function.
  x$setInv(m)  # Set the inverse result to the object.
  m  # Return the solution!
}
