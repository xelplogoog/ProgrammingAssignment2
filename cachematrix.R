## Caching the Inverse of a Matrix

## creates a cacheble 'matrix'

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inv) inverse <<- inv
	
	getinverse <- function() inverse
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## solves the special 'matrix'

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	
	if(!is.null(i)) {
			message("getting cached matrix")
			return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
