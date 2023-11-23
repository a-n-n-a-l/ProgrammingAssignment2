## The following functions take a matrix as an input, calculate its inverse and cache it globally


# This function creates a global environment variable for the inverse for the matrix
# and sets and gets the matrix and its inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            #initialize an empty variable
  
  set <-
    function(y) {
      #set the value of the matrix in a global variable
      x <<- y
      inv <<- NULL
    }
  
  get <- function()
    x                  #return the current matrix
  
  setInverse <-
    function(invMatrix)
      inv <<- invMatrix     #get the inverse matrix value
  
  getInverse <-
    function()
      inv                   #return the inverse matrix value
  
  list(
    #create a list with named values from the function, to make it easier to access
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


# This function checks to see if there's a cached version of the inverse of the matrix and return it
# or calculates an inverse if one doesn't yet exist in the cache

cacheSolve <-
  function(x, ...) {
    # Return the inverse of the matrix 'x'
    inv <-
      x$getInverse()                      #retrieve the inverse matrix from the passed object
    
    if (!is.null(inv)) {
      #check if there's a matrix already cached
      
      message("Getting cached data:")
      #alert the user that the matrix that will be returned has already been cached
      
      return(inv)           #return cached data
    }
    
    data <- x$get()
    inv <-
      solve(data, ...)              #calculate the inverse matrix of the passed object
    x$setInverse(inv)
    
    inv                                   #return the calculated inverse
    
  }