## cacheSolve and makeCacheMatrix compute the inverse of a matrix and cache the 
## result. subsequent requests for the inverse are looked up in the cache, if 
## the cache is null the inverse is computed and cache updated with the result


## makeCacheMatrix returns a modified matrix that include getter and setter
## functions that get and set the matrix x and/or get and set the inverse of x
## if computed
 
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(invs) i <<- invs
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an makeCacheMatrix argument x and returns the inverse
## of x first by checking the cache and if not cached, computes the inverse,
## and sets the cache for future requests

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)){
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(data,...)
		x$setinverse(i)
		i
}
