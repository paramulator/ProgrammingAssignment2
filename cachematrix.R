##------------------------------------------------------------------------------
## This program covers programming assignment #2 of the R Programming course.
##
## This exercise creates a special kind of matrix object that retains
## properties of itself, its inverse in particular (others could be added).
##
## If the matrix object is inserted into a special matrix inverse function, 
## and if a cached version of the inverse is already available, simply return
## the cached version.
## But, if the special matrix is new and no inverse has been calculated, go
## ahead and calculate the new inverse, store the inverse as a property of the 
## original special matrix, and return the new inverse to the caller.
##------------------------------------------------------------------------------


## Create a copy of the special matrix object.
## If no argument is supplied, an empty matrix is created and its inverse
## is set to NULL.
## The special matrix object has functions to update itself, return its value,
## and set and return its inverse value.
## These functions are returned to the caller.

makeCacheMatrix <- function(cache_mat = matrix()) {
    inv_mat <- NULL # When I use makeCacheMatrix explicitly to build a new
                    # matrix, I automatically set its inverse property to NULL.
    message("new matrix: setting its inverse to NULL")
    
    # Function to set (store) the current matrix and set its inverse to NULL.
    set <- function(y) {
        cache_mat <<- y
        inv_mat <<- NULL
        message("new matrix: setting its inverse to NULL")
    }
    
    # Function to get (return) the current matrix elements.
    get <- function() {
        cache_mat
    }
    
    # Function to set (store) the inverse of the current matrix.
    setinverse <- function(inv_matrix) {
        inv_mat <<- inv_matrix
    }
    
    # Function to get (return) the inverse of the current matrix.
    getinverse <- function() {
        inv_mat
    }
    
    # Return the list of functions for getting/setting the matrix and
    # its inverse. Name of each element in the list is the name of the function.
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## This function can return the inverse of a special matrix object.  If the
## matrix object has its inverse cached, the cached version is returned.
## But, if the matrix object does not have a cached inverse, the inverse
## calculated, cached, and then returned.

## Note:  this logic does not trap cases where an inverse is requested
## for a non-square matrix.  Other conditions that could cause the inverse
## to fail or not exist are not trapped either.

cacheSolve <- function(input_mat, ...) {
    # Get the cached copy of the inverse, if there is one.
    mat_inv <- input_mat$getinverse()
    
    # If there is a cached copy, do nothing else.
    if (!is.null(mat_inv)) {    
        message("getting cached inverse")
    
    # If there isn't a cached copy, generate one and cache it.
    } else {    
        message("calculating new inverse")
        mat_copy <- input_mat$get()
        mat_inv <- solve(mat_copy, ...)
        input_mat$setinverse(mat_inv)
    }
    
    # Return the inverse to the caller.
    mat_inv
}



### TEST CASES:

# TEST 1:  Make a new matrix based on an exisiting one:
b <- matrix(rnorm(64), 8, 8)
print(b)
c <- makeCacheMatrix(b)
print(c$get())
c_inv <- cacheSolve(c)
print(c_inv)
c_inv_copy <- cacheSolve(c)
print(c_inv_copy)


# TEST 2:  Make a new, blank copy of the special matrix and request its inverse.
a <- makeCacheMatrix()  # This call sets the inverse to NULL every time.
print(a$get())
a_inv <- cacheSolve(a)
print(a_inv)
a_inv_repeat <- cacheSolve(a)
print(a_inv_repeat)


# TEST 3:  Now change matrix 'a' by giving it new data.
a$set(matrix(rnorm(25), 5, 5))  # $set sets the inverse to NULL every time.
print(a$get())
a_inv <- cacheSolve(a)
print(a_inv)
a_inv_repeat <- cacheSolve(a)
print(a_inv_repeat)


# TEST 4:  Verify that the inverse of a is correct:
a$get() %*% a_inv  # Produces identity within tolerance.

