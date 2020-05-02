## the following functions cache the inverse of matrix

## this function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	cacheMatrix <- NULL
	setMatrix <- function(y) {
		x <<- y
		cacheMatrix <<- NULL
	}
	getMatrix <- function() x
	setCache <- function(inverse) cacheMatrix <<- inverse
	getCache <- function() cacheMatrix
	list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setCache = setCache,
           getCache = getCache)
}

## this function computes or retrieves the inverse of the matrix

cacheSolve <- function(x, ...) {
	cacheMatrix <- x$getCache()
	if (!is.null(cacheMatrix)) {
		message("loading cache matrix...")
		return(cacheMatrix)
	}
	else {
		dMatrix <- x$getMatrix()
		cacheMatrix <- solve(dMatrix, ...)
		x$setCache(cacheMatrix)
		return(cacheMatrix)
	}
}
