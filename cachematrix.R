## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize cache
	inv <- NULL
	
	set <- function(y) 
	{
		x <<- y
		inv <<- NULL
	}
  
	get <- function() x
  
	setinverse <- function(inverse) inv <<- inverse
	
	## Return cached matrix if set already else null	
	getinverse <- function() inv
  
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	## First check if cahced matrix is available
	inv <- x$getinverse()

    if(!is.null(inv)) 
    {
      # Found cached matrix
      return(inv)
    }
  
    ## Cache not available, this compute inverse
	inv <- solve( x$get() )
	
	## set the inverse so that it can be cached
	x$setinverse(inv)
	
	inv
}
