## The two functions in this file are used to create a special object
## that stores a square invertible matrix and cache's its inverse. 

## This function creates a special matrix object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Creates the invMat variable in the environment of the 
    ## makeCacheMatrix function.
    invMat <- NULL
    
    ## Set new values for the matrix object using the superassignment
    ## operator and resets its inverse.
    set <- function(mat){
        x <<- mat
        invMat <<- NULL
    }
    
    ## One line function to get the current values of the matrix object.
    get <-function() x
    
    ## One line function to set the inverse of the matrix using the 
    ## superassignment operator.
    setInv <- function(inv) invMat <<- inv
    
    ## One line function to get the inverse of the matrix
    getInv <- function() invMat
    
    ## Output list of the makeCacheMatrix function
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInv()
      
      ## Conditional statement if the inverse is allready stored in
      ## the cache, in order to use its stored value and avoid
      ## recalculation.
      if(!is.null(invMat)){
          message("Getting cached data")
          return(invMat)
      }
      
      ## Get the matrix, calculates its inverse and cache's the result
      data <- x$get()
      invMat <- solve(data)
      x$setInv(invMat)
      
      ## Returns the inverse
      invMat
}
