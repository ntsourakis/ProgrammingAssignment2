## This file contains two functions for calculating the 
## inverse of a matrix. This matrix can also be cached
## for efficiency purposes.
##
## 'makeCacheMatrix' function creates a structure that 
## can cache the inverse of a matrix
## 
## 'cacheSolve' function is used to calculate the inverse 
## of a matrix or retrieve it from the cache structure
##

## Function: makeCacheMatrix 
## Input:	 A square matrix x
## Output: 	 A list of functions for:
## 			a) setting the value of the matrix
## 			b) getting the value of the matrix
## 			c) setting the value of inverse of the matrix
## 			d) getting the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Function: cacheSolve
## Input:	 An invertable matrix x
## Output: 	 The inverted matrix of x

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	
	## Is the inverse matrix already computed?
	if(!is.null(inv)) {
		message("getting inverse matrix")
		return(inv)
	}
		
	data <- x$get()

	## Compute the inverse matrix and set the value in the cache
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
