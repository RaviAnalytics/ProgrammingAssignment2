# name: Ravi Ancil Persad
# Coursera: R-programming/Week 3 Assignment

## Two functions are created here: 1) makeCacheMatrix and
## 2) cacheSolve. 

## The first function 'makeCacheMatrix' 
## generates a 'special' matrix object containing a function to:
## a)set the value of the matrix, b)get the value of the matrix, 
## c) set the value of the matrix inverse and d) get the value of
## the matrix inverse. 

## The second function 'cacheSolve' computes
## the inverse of the 'special' matrix. However, if the matrix 
## inverse is already computed, the 'cacheSolve' function obtains 
## the inverse from the cache and does not do any calculation.

# ======= Assignment code starts from here ==========#

# The makeCacheMatrix function creates a 'special' matrix.

# Step 1: let's create the makeCacheMatrix function:

makeCacheMatrix <- function(A = matrix()) {
        
        matrix_inverse <- NULL # this pre-sets the matrix inverse variable
        set <- function(B) {
                A <<- B
                matrix_inverse <<- NULL
        }
        get <- function() A
        set_mat_inv <- function(set_mat_inv) matrix_inverse <<- set_mat_inv
        get_mat_inv <- function() matrix_inverse
        list(set = set, get = get, 
             set_mat_inv = set_mat_inv, get_mat_inv = get_mat_inv)
        
}

# The cacheSolve function computes the inverse of the generated
# 'special' matrix. If the inverse is already computed, this 
# function can retrieve it from the cache.

# Step 2: let's create the cacheSolve function:

cacheSolve <- function(A, ...) {
        matrix_inverse <- A$get_mat_inv()
        if(!is.null(matrix_inverse)) {
                message("Retrieving Cached result")
                return(matrix_inverse)
        }
        Matrix <- A$get()
        matrix_inverse <- solve(Matrix)
        A$set_mat_inv(matrix_inverse)
        matrix_inverse
}


# Step 3: let's test to see if the function works properly:

A = matrix(c(3,4,1,2),nrow=2,ncol=2) # this is a 2x2 matrix

B = makeCacheMatrix(A)

C1 = cacheSolve(B)
C1 # show computed result

C2 = cacheSolve(B)
C2 # show cached result

# ============= End of Assignment =================#
