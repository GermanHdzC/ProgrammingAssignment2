rm(list = ls())

makeCacheMatrix <- function(ma = matrix()) {
im <- NULL
setMatrix <- function(y) {
ma <<- y
im <<- NULL
}
getMatrix <- function() ma
setinverse <- function(inv) im <<- inv
getinverse <- function() im
list(setMatrix = setMatrix,
getMatrix = getMatrix,
setinverse = setinverse,
getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
im <- x$getinverse()
if (!is.null(im)) {
message("getting cached inverse matrix")
return(im)
}
data <- x$getMatrix()
i <- solve(data, ...)
x$setinverse(i)
i
}
