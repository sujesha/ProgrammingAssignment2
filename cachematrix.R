## makeCacheMatrix() takes as input a matrix and gives as output a special "list"
## of functions that can be used to set(), get() the value within (x) and also 
## set and get the inverse of x
## Note that getinverse() returns a valid value only if it has been computed already.
## cacheSolve() takes as input the "special" matrix object which was returned by
## makeCacheMatrix(), checks whether inverse is already present, and
## returns it if present. If inverse is not present, this implies that previously
## a "new" matrix was set using set() invocation.


## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

		## create 'i' to cache the inverse
		i <- NULL

		## 'set' function to store the input matrix into x
		## Additionally, while set(), check that new matrix is different from previous,
		##	if they are same, then no need to set here.

		set <- function(y) {

			## if there's previously stored matrix, check if it is same as current input
			if (!is.null(x) & identical(x,y))
			{
				## do nothing if input matrix is same as previously stored matrix
				message("do nothing since matrix is same in set()")
			}
			else
			{
				message("setting the matrix since it's different")
		        x <<- y
		        i <<- NULL
			}
		}

		## 'get' function to retrieve the previously stored matrix
		get <- function() x

		## 'setinverse' function to store inverse of matrix, this also uses
		##	the <<- operator, so inverse will be stored in variable 'i' searched
		#	for in enclosing environments
		setinverse <- function(inverse) i <<- inverse

		## 'getinverse' function to retrieve the previously stored inverse
		getinverse <- function() i

		## returns a list of the set, get, setinverse, getinverse functions as 
		## output of makeCacheMatrix() function
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
## This function computes the matrix inverse of the matrix returned by 
## makeCacheMatrix(). If the inverse has already been calculated (and 
## matrix hasn't changed, then return the cached inverse from before.
cacheSolve <- function(x) {

		## if input matrix is same as previous matrix, inverse will be present
		## due to check done in set() above
		i <- x$getinverse()
		if(!is.null(i)) {
   	        message("getting cached inverse since matrix is same")
		    return(i)
		}

		## If we are here, there was no previous inverse cached,
		## so compute matrix inverse of the given input matrix, 
		## and store the inverse for future use.
		inverse <- solve(x$get())		## computes inverse using solve()
		x$setinverse(inverse)	## save inverse
		inverse					## return inverse as output of cacheSolve() function
}
