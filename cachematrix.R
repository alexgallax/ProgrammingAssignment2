# Function provide 4 functions:
# getter method 'get' to get matrix we want to inverse
# getter method 'getsolve' to get cached value of inversed matrix
# 'set' sets matrix to method's argument and sets cache to NULL in parent environment
# 'setsolve' to write inversed matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    stored <- NULL                                  # Set cache to NULL
    
    set <- function(y) {                            # Function that
        x <<- y                                     # sets matrix to its argument
        stored <<- NULL                             # and sets cache to NULL
    }                                               # for parent environment
    
    get <- function() x                             # Function that returns matrix -
                                                    # - getter method for matrix
    setsolve <- function(solved) stored <<- solved  # Function that takes matrix (inversed) as argument
                                                    # and put it in cache for parent environment
    getsolve <- function() stored                   # Function that returns cached value -
                                                    # - getter method for matrix cache
    list(set=set, get=get,                          # Return the list of functions
         setsolve=setsolve, getsolve=getsolve)      # implemented in makeCacheMatrix
}


# Function first get cached value of inversed matrix using 'getsolve'
# that check if we have cached value (not null) or not (is null)
# If we have cached value for inversed matrix we return it and exit the function
# If not, we get original matrix using 'get', inverse it with standard 'solve' function,
# write it in cache with 'setsolve'
# and return evaluated result - inversed matrix

cacheSolve <- function(x, ...) {
    stored <- x$getsolve()                          # get cached value
    
    if (!is.null(stored)) {                         # check if we have cached value
        message("getting cached data")              # if we have cache
        return(stored)                              # return data from cache
    }
                                                    # if no cached value
    data <- x$get()                                 # get matrix
    stored <- solve(data, ...)                      # inverse matrix
    x$setsolve(stored)                              # and cache it
    stored                                          # return inverse matrix
}
