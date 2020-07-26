rm(list = ls())

#makeCacheMatrix consist of set, get, set inv, get inv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#This is used to get the cache data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){ #Checking whenever the inverse is null
    message("getting cached data")
    return(inv)  #return inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
