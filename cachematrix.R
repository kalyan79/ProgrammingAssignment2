## The below functions are written to calculate Inverse of Matrix and store it in Cache to avoid recalculating matrix inverse every time.
## Function will check if inverse matrix is already exists in cache and use it . If not it would calculate and store it in cache for future reference 

## Function set the value of matrix, get the value of matrix , set inverse of matrix in cache and 
## get inverse of matrix from cache


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function (y) {
		x <- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(solve) i<<- solve
	getinverse <- function () i
	
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## Function checks if inverse of matrix already exists in the cache and return. If inverse not
## found in  cache then it would calculate inverse and set the inverse in cache for further references.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting Data from Cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
