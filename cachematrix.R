## Caching the inverse of a matrix (matrix inversion is time consuming, and catching the inverse of a matrix ranther than computing it repeatedly is beneficial)

## below is a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse; cacheSolve computes the inverse of the special matrix returned by makeCacgeMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y){
			x<<-y
			inverse<<-NULL
		}
		get <- function() x
		setInverse <- function(inverse)inverse<<- inverse
		getInverse <- function() inverse
		list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)){
        	message("getting cached data")
        	return (inverse)
        }
        matrix <- x$get()
        inverse<-solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
