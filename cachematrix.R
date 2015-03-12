## Since calculating the inverse of a matrix can be potentially
## a time-consuming task, if the matrix is not changing and we
## are calculating its inverse many times, it might be a good
## idea to calculate the inverse only once and cached it. So 
## we only recalculate the inverse matrix when the matrix is
## changed.
## The approach used in this example is creating an R object
## that can save the state of the matrix and its inverse. This
## object allows us to get and set those values. Then we have
## a function CacheSolve that works with that R object to get
## the inverse if it is already calculated, or to calculate if
## not

## This function creates an R object that can track the state
## of the matrix and its inverse. It returns a list with four
## functions: the getter and setter of the matrix, and the
## getter and setter of the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Cached inverse matrix of X
    inverse <- NULL
    
    # Getter and setter for X   
    get <- function() x
    
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    # Getter and setter for the inverse matrix of x    
    get_inverse <- function() inverse
    
    set_inverse <- function(inverted_x) inverse <<- inverted_x
    
    # Return the list encapsulating getters and setters
    list (set = set, get = get, 
          set_inverse = set_inverse,
          get_inverse = get_inverse)

}


## This function first looks for a cached copy of the
## inverted matrix. If it exists, then the program
## returns it. Otherwise, it calculates it, caches it
## and returns it.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    # Check if there is a cached copy of the inverse
    if (!is.null(inverse)){
        message("Getting cached data")
        # Return the cached copy.
        return (inverse)
    }
    # There is no cached copy, so calculate the inverse.
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set_inverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse  
}
