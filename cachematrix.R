## Timothy Johnstone, April 2015 
## Programming Assignment 2
## Data Science Course 2: R Programming (rprog-013)
 

# The following functions implement a special matrix object that allows caching of
# the matrix inversion, so that less computation is required upon subsequent
# requests for inversion, and an inversion method that is aware of this caching.
# The caching takes advantage of R's lexical scoping to modify and access objects
# in parent environments of a given object environment.

##########################

# This function creates a CacheMatrix capable of caching its inverted form. 
# Each CacheMatrix contains four accessible functions:
# get and set retrieve and modify the value of the 'matrix'
# getinverse and setinverse retrieve and modify the value of the inverse/solution

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL			# stores the inverted matrix
	# get/set
	get <- function() x
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# get/set for inversion
	getInverse <- function() inv
	setInverse <- function(solve) inv <<- solve
	# return the 'CacheMatrix'
	return(list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse))
}

##########################

# This function calculates the inverted form of a supplied CacheMatrix in a cache-aware
# manner. If the inverse is not cached, cacheSolve will calculate and store it. If 
# the inverse is cached, cacheSolve will also notify the user that it is returning
# a cached value.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	# Use cached inversion if there
	if(!is.null(inv)) {
			message("Using cached inverted matrix")
			return(inv)
	}
	# Otherwise, calculate it and store as inv
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	return(inv)
}
