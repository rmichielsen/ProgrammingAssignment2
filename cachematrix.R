## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix value
    inverse <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    set_inverse <- function(inv_input) inverse <<- inv_input
    # get the value of the inverse
    get_inverse <- function() inverse
    
    # return a list of all the above functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inverse function

cacheSolve <- function(x, ...) {
    # check if the inverse is already cached,
    # if it is available in cache, retrieve the inverse value from there
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    # if not, get the matrix, 
    data <- x$get()
	# calculate the inverse, 
    inverse <- solve(data, ...)
    # and store it into the cache 
    x$set_inverse(inverse)
    # return the inverse
    inverse
}
