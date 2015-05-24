## The following two functions can be used to avoid repeatedly 
## calculating the inverse of matrices, by providing methods to
## cache the inverse. 



## Creates a special "matrix" object capable of holding a cached 
## inverse of a matrix, with getter and setter methods for the 
## wrapped matrix and for the inverse.
##
## Arguments:
## x: a square invertible matrix
## 
## Returns:
## the special "matrix" objects with above described methods

makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL	# the cached value for the inverse 

	# sets the value of the matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# gets the value of the matrix
	get <- function() {
		x
	}	

	# stores the value of the inverted matrix
	setInverse <- function(i) {
		inverse <<- i	
	}

	# gets the value of the inverted matrix
	getInverse <- function() {
		inverse
	}

	# returns the list of created functions
	list (set = set, 
		get = get,
		setInverse = setInverse,
		getInverse = getInverse) 
	
}


## Checks whether a matrix object created via "makeCacheMatrix()" has
## a cached value for an inversed matrix. If a non-null value is found,
## it is returned, otherwise the inverse is calculated and stored in 
## the cache. 
##
## Arguments:
## x: a matrix object capable of caching its inverse
##
## Returns:
## a matrix that is the inverse of "x", either from the cache, or 
## calculated by calling "solve()" 

cacheSolve <- function(x, ...) {
	
	# attempt to get the inverse from the cache
	inverse <- x$getInverse()

	# if a not null value is found, return it
	if (!is.null(inverse)) {
		message("Returning cached inverse")
		return(inverse)
	}

	# no cached value found, calculate the inverse
	# TODO: error handling?
	inverse <- solve(x$get())

	# cache the calculated value
	x$setInverse(inverse)

	inverse
}
