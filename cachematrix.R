## The functions below are used to create a special object that stores a 
## matrix and caches its inverse

## makeCacheMatrix returns a special "matrix", which is a list that
## contains functions to set and get the values of the matrix and its
## inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(i) inv <<- i
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve makes use of the special "matrix" created in makeCacheMatrix
## to either get the cached matrix inverse (if it's already been computed)
## or calculate it (if otherwise)

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
            message("getting cached data")
      	return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInv(inv)
      inv
}
