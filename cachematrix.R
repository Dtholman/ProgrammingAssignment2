## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
	set <- function(y) {
		x <<- y
		a <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) a <<- inverse
	getinverse <- function() a
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## This function computes the inverse of a special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated then the 
## cache should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	a <- x$getinverse()
	if(!is.null(a)) {
		message("getting cached data")
		return(a)
	}
	data <- x$get()
	a <- solve(data, ...)
	x$setinverse(a)
	a
}