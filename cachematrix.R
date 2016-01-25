## Matrix inversion is usually a costly (i.e. time-consuming) computation.  There may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. 
##	
## The following pair of functions will cache the inverse of a matrix.  
## 	1: makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
##	2: cacheSolve: Computes the inverse of the special "matrix" returned by the makeCacheMatrix function above.
##		If the inverse has alraedy been calculated (and the matrix has not changed), then the cacheSolve
##		should retrieve the inverse from the cache.

## The first function, makeCacheMatrix, creates a special "matrix", which is a list containing a function to:
##	1. set of the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse of the matrix
##	4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inversedMatrix <- NULL
	set <- function(y) {
		x <<- y
		inversedMatrix <- NULL
	}
	get <- function() x
	setinverse <- function(solve) inversedMatrix <<- solve
	getinverse <- function() inversedMatrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## The second function, cacheSolve, computes the inverse of the special "matrix" created with the makeCacheMatrix function above. 
## However, it first checks to see if the inverse has already been computed. If so, it gets the inverse of the matrix from the cache
## and skips the computation. Otherwise, it computes the inverse of the matrix and sets the inverse of the matrix in the cache via
## the setinverse function. 

cacheSolve <- function(x, ...) {
	inversedMatrix <- x$getinverse()
	if(!is.null(inversedMatrix)) {
		message("getting cached data")
		return(inversedMatrix)
		}
		data <- x$get()
		inversedMatrix <- solve(data)
		x$setinverse(inversedMatrix)
        ## Return a matrix that is the inverse of 'x'
		inversedMatrix
	}
