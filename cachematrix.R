## This functions create an object-like matrix and calculate the inverse of the matrix stored 
## in this object-like list. 

## This function creates a list of functions able to store and show a matrix and its inverse.
## WARNING: the inverse is not calculated so any matrix can be stored in its place.

makeCacheMatrix <- function(x = matrix()) {

	# The inverse matrix is set to null
	im <- NULL

	# The set function stores a new matrix into the list and resets
	# the inverse matrix to null
      set <- function(y) {
      	x <<- y
            im <<- NULL
      }

	# get returns the matrix 
      get <- function() x

	# The inverse matrix is stored, but not calculated
      setinverse <- function(inverse_matrix) im <<- inverse_matrix

	# getinverse returns the inverse of the matrix
	getinverse <- function() im
      
	# The returned variable is a list of functions 
      list(set = set, get = get,
      	setinverse = setinverse ,
            getinverse = getinverse )
}


## This function calculates the inverse of the matrix stored via the function set of the 
## list created by makeCacheMatrix. The inverse is only calculated only there is nothing
## stored in the memory reserve to it.
## WARNING: the matrix is not checked to assure if it is invertible
## WARNING: in case something s stored in the memory reserved to the inversed matrix it
## is not checked if it is a the inverse of the matrix or if it is even a matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      im <- x$getinverse()

	# If the matrix already exist it is simply returned.
      if(!is.null(im)) {
      	message("getting cached data")
            return(im)
      }

	# Else the inverse is calculated, stored into memory, and returns the value
      data <- x$get()
      im <- solve(data, ...)
      x$setinverse (im)
      im

}
