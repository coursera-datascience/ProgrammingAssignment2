## Description--------------
## Two customized functions that cache the inverse of a matrix.

# 1. makeCacheMatrix

# Description --------------
# The function creates a special "matrix" object that can cache its inverse.

# Usage--------------------
# makeCacheMatrix(a)

# Arguments----------------
# a    a square numeric or complex matrix. Logical matrices are coerced to numeric.

# Examples-----------------
# mx <- rbind(c(1,3), c(5, 6))
# makeCacheMatrix(mx)
#
# mx <- rbind(c(1,3), c(5, 6))
# cashed <- makeCacheMatrix(mx)

makeCacheMatrix <- function(x = matrix()) {        
        inv <- NULL               # Object to hold the value of an inversed matrix
        set <- function(y) {      
                x <<- y           # Assign the value of the matrix to the object x in the parent environment
                m <<- NULL        # Assign NULL to an object in the parent environment
        }
        get <- function() {       # Get the value of the matrix
                x
        }
        setinverse <- function(solve){ # Set the value of the inversed matrix
                inv <<- solve     # Assign the inversed to an object in the parent environment
        }                             
        getinverse <- function(){ # Get the value of the inversed matrix
                inv
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# 2. cacheSolve

# Description --------------
# The function computes the inverse of the special "matrix".
# If the inverse has already been calculated and the matrix has not changed, 
# the cacheSolve function retrieves the inverse from the cache.

# Usage--------------------
# cacheSolve(a, ...)

# Argument----------------
# a    a cashed object in the environment which calls the cacheSolve function
#...   further arguments passed to or from other methods

# Examples-----------------
# cacheSolve(cashed)

cacheSolve <- function(x, ...) {  # A cashed object in the parent frame is passed to the function
        inv <- x$getinverse()     # Retrieve the inversed matrix from the object passed from the parent frame
        if (!is.null(inv)){       # If the object contains an inversed matrix
                message("getting cached data")
                return (inv)      # retrieve the inversed matrix and exit the function
        }                       
        
        # Lines below will be reached ONLY when no inversed matrix exists
        data <- x$get()           # Get the matrix from the object passed from the parent frame 
        inv <- solve(data, ...)   # Inverse the data matrix
        x$setinverse(inv)         # Set the inversed matrix to the object passed from the parent frame
        inv                       # Return an inversed matrix
}