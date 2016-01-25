## makeCacheMatrix creates sepcial matrix object that can cache a matrix

## cacheSolve inverts the special matrix object returned by makeCacheMatrix. If the inverse is already calculated, 
## then cacheSolve retrieves the inverse from the cache. If the inverse is not available, cacheSolve uses solve 
## function to invert the matrix 



## Creates a sepcial matrix object that can cache a matrix

makeCacheMatrix <- function(x = matrix()) {
	Invertedmatrix <- NULL
	set <- function(y) {
		x <<- y
		Invertedmatrix <<- NULL
	}
	get <- function() x
	setmatrix <- function(si) Invertedmatrix <<- si
	getmatrix <- function() Invertedmatrix 
	list(set = set, get = get,
	setmatrix = setmatrix,
	getmatrix = getmatrix)
}


## Returns a matrix that is the inverse of the matrix created by the makeCacheMatix function. if the inverse is
## available, it will return the inverse from the cache. If not, use solve to invert the matrix

cacheSolve <- function(x , ...) {
	invmat <- x$getmatrix()
	if (!is.null(invmat)) {
		message("getting cached data")
		return(invmat)
	}

	origmatrix <- x$get()
	invmat <- solve(origmatrix, ...)
	x$setmatrix(invmat)
	invmat
}
