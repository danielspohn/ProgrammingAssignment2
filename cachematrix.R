## The makeCacheMatrix function and the cacheSolve function work
## together to calculate and store a matrix invese into memory
## so that it doesn't have to be recomputed later

## The makeCacheMatrix function will take in a matrix and will make 
## it a special matrix which is actully a list,
## and then it appending functions to it that will get (only if it's already set) 
## and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #set m (which is matrix inverse) to NULL
  m <- NULL 
  set <- function(y) {
    #setting x (which is the matrix, to the newly supplied matrix)
    x <<- y 
    #set the m in parent function( makeCacheMatrix ) to NULL
    m <<- NULL 
  }
  
  #get will return the matrix
  get <- function() x 
  
  #set inverse by supplied value
  setinverse <- function(inverse) m <<- inverse 
  
  #return the inverse value that is set
  getinverse <- function() m 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## the cacheSolve function will solve the inverse of the matrix, if it has
## not yet been solved. If the inverse of the matrix has already been solved
## then it will just returned the cached solved value.

cacheSolve <- function(x, ...) {
  #get the cached inverse value (may be NULL if not yet solved for)
  m <- x$getinverse()
  
  #if inverse has already been solved for (ie it's not null) 
  #then return the cached version
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if inverse has not been solved for get the matrix and store in data variable
  data <- x$get()
  
  #solve for the inverse of the matrix
  m <- solve(data, ...)
  
  #set the cache for the inverse of the matrix to the inverse
  x$setinverse(m)
  
  #return the inverse of the matrix
  m
}



