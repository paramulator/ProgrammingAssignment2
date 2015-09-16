##------------------------------------------------------------------------------
## This program covers assignment #2 of the R Programming course.
##------------------------------------------------------------------------------
## This exercise creates a special kind of matrix object that retains
## properties of itself, its inverse in particular (others could be added).
##
## The special matrix object is really a list of functions that can return  
## properties of the object.
##
## If the matrix object is inserted as the argument into a special matrix 
## inverse function, and if a cached version of the object's inverse is already 
## available, the function will simply return the cached inverse.  But, if the 
## special matrix is new and no inverse has been calculated yet, the inverse
## function will calculate the new inverse, store the inverse as a property of 
## the original special matrix object, and return the new inverse to the caller.
##------------------------------------------------------------------------------





##------------------------------------------------------------------------------
## function: makeCacheMatrix
##------------------------------------------------------------------------------
## Purpose: Creates an instance of the special matrix object (a list).
## If no argument is supplied, an empty matrix is created and its inverse
## is set to NULL.
##------------------------------------------------------------------------------
## The special matrix object has functions to update itself, return its value,
## and set and return its inverse value.
## These functions are returned to the caller.
##------------------------------------------------------------------------------
## The special matrix object can be created by making a call directly to 
## this function i.e. a <- makeCacheMatrix(matrix(rnorm(64), 8, 8)) 
##
## Once created, 'a' can be updated i.e. a$set(matrix(rnorm(25), 5, 5))
##------------------------------------------------------------------------------
## This function illustrates the concept of property 'setters' and 'getters.'
##------------------------------------------------------------------------------

makeCacheMatrix <- function(cache_mat = matrix()) {
    inv_mat <- NULL # When makeCacheMatrix is used explicitly to build a new
                    # matrix, automatically set its inverse property to NULL.
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
    # its inverse. The name of each element in the list is the function name.
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



##------------------------------------------------------------------------------
## function: makeCacheMatrix
##------------------------------------------------------------------------------
## Purpose: Return the inverse of a special matrix object.  If the
## matrix object has its inverse cached, the cached version is returned.
## But, if the matrix object does not have a cached inverse, the inverse
## calculated, cached, and then returned.
##------------------------------------------------------------------------------
## Note: This logic does not trap cases where an inverse is requested
## for a non-square matrix.  Other conditions that could cause the inverse
## to fail or not exist are not considered either.
##------------------------------------------------------------------------------

cacheSolve <- function(input_mat, ...) {
    # Get the cached copy of the inverse, if there is one.
    mat_inv <- input_mat$getinverse()
    
    # If there is a cached copy, do nothing else.
    if (!is.null(mat_inv)) {    
        message("getting cached inverse")
    
    # Else, if there isn't a cached copy, generate one and cache it.
    } else {    
        message("calculating new inverse and caching it")
        mat_copy <- input_mat$get()
        mat_inv <- solve(mat_copy, ...)
        input_mat$setinverse(mat_inv)
    }
    
    # Return the inverse to the caller.
    mat_inv
}



### TEST CASES:

# TEST 1:  Make a new special matrix object based on an exisiting std. matrix:
b <- matrix(rnorm(64), 8, 8) # Standard square matrix.
print(b)
c <- makeCacheMatrix(b) # Special matrix object with NULL inverse. 
print(c$get())
c_inv <- cacheSolve(c)  # Calculate inverse for the first time and cache it.
print(c_inv)
c_inv_copy <- cacheSolve(c)  # Retrive cached inverse. 
print(c_inv_copy)


# TEST 2:  Make a new, blank copy of the special matrix and request its inverse.
a <- makeCacheMatrix()  # Empty matrix with NULL inverse cached.
print(a$get())  
a_inv <- cacheSolve(a) # Attempt to compute the inverse (even though its NA.)
print(a_inv) 
a_inv_repeat <- cacheSolve(a) # Retrives the cached version, which is just NA.
print(a_inv_repeat) 


# TEST 3:  Now change matrix 'a' by giving it new data.
a$set(matrix(rnorm(25), 5, 5))  # $set sets the inverse to NULL every time.
print(a$get())  
a_inv <- cacheSolve(a)  # Computes the inverse for the first time and caches it.
print(a_inv)  
a_inv_repeat <- cacheSolve(a) # Retrieve the inverse from cache.  
print(a_inv_repeat)


# TEST 4:  Verify that the inverse of a is correct:
a$get() %*% a_inv  # Produces identity within tolerance.

