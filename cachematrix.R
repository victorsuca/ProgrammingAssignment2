## Construct the inverse of a matrix

## Create a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	
	set <- function( matrix ) {
		x <<- matrix
		m <<- NULL
	}

	get <- function() x
	
	setInvesrse <- function(inverse) m <<- inverse
  	
	getInverse <- function() m

	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	n <- x$getInverse()

	if( !is.null(n)) {
		message("getting cache data")
		return(n)
	}

	data <- x$get()
	
	n <- solve(data) %*% data

	x$setInverse(n)

	n
	
}
