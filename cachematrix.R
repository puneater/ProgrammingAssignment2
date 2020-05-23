## Programming Assignment 2 for the course R programming:
## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that
## we will not discuss here). Your assignment is to write a pair of
## functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL  # 'i' stores inverse of the matrix
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	getinverse <- function() i
	setinverse <- function(inverse) i <- inverse
	list(set = set, get = get, 
	     getinverse = getinverse,
	     setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned
## by the function makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x) {
      ## Returns a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}