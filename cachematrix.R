## 'makeCacheMatrix' function creates a stucture that 
## can cache the inverse of a matrix
## 
## 'cacheSolve' function is used to calculate the inverse 
## of a matrixor or retrieve it from the cache
##

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	
	if(!is.null(i)) {
		message("getting inverse matrix")
		return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
