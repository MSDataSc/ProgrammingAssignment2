## makeCacheMatrix() returns a matrix that has the following functions
## get() - Returns the Matrix
## set() - sets the matrix that was passed in and resets the cached matrix (initialize)
## setIn() - Sets the cache using the matrix that was passed in
## getIn() - Retruns the cached Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # Create/Initialize m as to store the cached copy of the matrix
  
  ## set() Function to set the matrix passed in as the working matrix (x) 
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get() Return x (working copy of the matrix)  
  get <- function() x
  
  ## setIn() - Set the matrix passed on as the cached matrix
  setIn <- function(a) m <<- a
  
  ## getIn() - Return the cached matrix
  getIn <- function() m

  ## Return the set(), get(), setIn(), getIn() as the functions of the set Matrix
  list(set = set, get = get, setIn = setIn, getIn = getIn)

}


## cacheSolve - Takes the input matrix and returns the inverse of the matrix passed in. 
## Returns the cached version if exist and creates an inverse matrix if a cached version does not exist

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m <- x$getIn()
        
        ## If the cache matrix is set (not null)
        if (!is.null(m)) {
          message("Getting from Cache")
          # return the cached matrix
          return(m)
        }
        ## no cached version found. Get the inverse of the matrix and set the cache before returning.
        t <- x$get()  # get teh matrix
        m <- solve(t) # create the inverse
        x$setIn(m)    # set the inverse in the cache
        return(m)     # return the inversed matrix
}
