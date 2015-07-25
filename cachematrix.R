

makeCacheMatrix <- function(x = matrix()) 
{

## makeCacheMatrix creates a special "invertible matrix" 
## It's list containing a function to
## set  values of the matrix
## get  values of the matrix
## set  values of the inverse of the matrix
## get  values of the inverse of the matrix

 set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
    
  get <- function () x
 
  setinverse <- function (inv)  m <<- inv
    
  getinverse <- function () m
  
  list(
  set = set, 
  get = get,
  setinverse = setinverse,
  getinverse = getinverse)

}



cacheSolve <- function(x, ...) {

## This function calculates the inverse of the special "invertible matrix" created 
##with the above function. However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
##of the matrix and sets the values of the inverse in the cache via the setinverse function


 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
