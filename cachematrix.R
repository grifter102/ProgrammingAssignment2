## These functions create a special object which stores a numeric matrix and has the ability to calculate and cache the vector's inverse

## This function creates the special object and its functions

makeCacheMatrix <- function(x = matrix()) {
    # default the inverse to NULL
    i <- NULL
    
    # create the set function
    set <- function(y) {
      # assign argument of set to the matrix object (x) of the parent object and reset the inverse (i)
      x <<- y
      i <<- NULL
    }
    
    #create the get function which returns the matrix object (x)
    get <- function() x
    
    #create the setinverse and getinverse functions to set and return the inverse objects of the parent
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    #return a list of all the functions to set/get the matrix object and inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will return the inverse of the matrix either pulling it from the cache or calculating it if no cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # pull the inverse from x
  i <- x$getinverse()
  
  #check if the inverse is already cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if the inverse is not cached, pull the matrix from x
  data <- x$get()
  
  #calculate the inverse via the solve function
  i <- solve(data, ...)
  
  #cache the inverse in the object
  x$setinverse(i)
  
  #return the inverse
  i
}
