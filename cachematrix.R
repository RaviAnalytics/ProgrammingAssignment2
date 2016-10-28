# name: Ravi Ancil Persad
# Coursera: R-programming/Week 3 Assignment

## Two functions are created here: 1) makeCacheMatrix and
## 2) cacheSolve. 



# ======= Assignment code starts from here ==========#

## The first function 'makeCacheMatrix' 
## generates a 'special' matrix object containing a function to:
## a)set the value of the matrix, b)get the value of the matrix, 
## c) set the value of the matrix inverse and d) get the value of
## the matrix inverse. 

# Step 1: let's create the makeCacheMatrix function:

makeCacheMatrix <- function(x = matrix()) {
        
        matrix_inverse <- NULL # this pre-sets the matrix inverse variable
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        set_mat_inv <- function(mat_inv) matrix_inverse <<- mat_inv
        get_mat_inv <- function() matrix_inverse
        list(set = set, get = get, 
             set_mat_inv = set_mat_inv, get_mat_inv = get_mat_inv)
        
}

## The second function 'cacheSolve' computes
## the inverse of the 'special' matrix. However, if the matrix 
## inverse is already computed, the 'cacheSolve' function obtains 
## the inverse from the cache and does not do any calculation.

# Step 2: let's create the cacheSolve function:

cacheSolve <- function(x, ...) {
        matrix_inverse <- x$get_mat_inv()
        if(!is.null(matrix_inverse)) {
                message("Retrieving Cached result")
                return(matrix_inverse)
        }
        Matrix <- x$get()
        matrix_inverse <- solve(Matrix)
        x$set_mat_inv(matrix_inverse)
        matrix_inverse
}


# Step 3: let's test to see if the function works properly:

A <- matrix(c(3,4,1,2),nrow=2,ncol=2) # this is a 2x2 matrix

B <- makeCacheMatrix(A)

C1 <- cacheSolve(B)
C1 # show computed result

C2 <- cacheSolve(B)
C2 # show cached result

# ============= End of Assignment =================#
