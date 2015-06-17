## Application of the two functions provides us the benefit of caching the 
## inverse of a matrix that has already been computed previously rather than 
## computing it repeatedly thus saving a lot of time.

## The function creates a special matrix type object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {

	## initializing the variable that will store the inverse to NULL
	i <- NULL

	## function to set the matrix whose inverse is to be computed
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## function to return the matrix whose inverse is computed
	get <- function() x

	## function to set the computed inverse matrix
	setinverse <- function(inverse) i <<- inverse

	## function to return the inverted matrix
	getinverse <- function() i

	## creating a list of the subsetted functions
	list(set = set, get = get, setinverse = setinverse, 
						getinverse = getinverse)

## The function computes the inverse of the matrix created using 
## makeCacheMatrix function and retrieves the inverse from the cache if it has 
## already been computed.

cacheSolve <- function(x, ...) {
      ## gets the inverted matrix if it exists
	i <- x$getinverse()

	## checking if the matrix have been inverted or not
	if(!is.null(i)) {
		## returning cached data if the inverted matrix has been computed
		## previously
		message("getting cached data")
		return(i)
	}

	## gets the matrix to be inverted
	data <- x$get()

	## computing the inverse of the matrix
	i <- solve(data, ...)

	## setting the inverted matrix so that it can be cached later	
	x$setinverse(i)
	
	## returning the inverted matrix
	i
}
