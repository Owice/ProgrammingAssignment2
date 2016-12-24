## Put comments here that give an overall description of what your
## functions do

#Basically these functions first check if the inverse of a certain matrix
#was calculated previously or not before calculating it. If it was calculated before,
#it will show the previous result without doing new calculation.
#This should disallow unnecessary repetition of the calculation.

## Write a short comment describing this function
#This function will create a list of functions which are
#responsible of setting and getting cached values.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		  set <- function(y) {
			x <<- y
			m <<- NULL
		  }
		  get <- function() x
		  setinverse <- function(result) m <<- result
		  getinverse <- function() m
		  list(set = set, get = get,
			   setinverse = setinverse,
			   getinverse = getinverse)
}


## Write a short comment describing this function
#This function checks if the inverse of the matrix was calculated before.
#If it was, it will bring the cached value and writing a message indicating that, 
#otherwise it will calculate and cach it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
				m <- x$getinverse()
		  if(!is.null(m)) {
			message("getting cached data")
			return(m)
		  }
		  a <- x$get()
		  m <- solve(a,...)
		  x$setinverse(m)
		  m
}
