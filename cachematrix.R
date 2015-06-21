## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##      makeCacheMatrix <- function(x = matrix()) {
##      x is a square matrix
##      return: a list containing functions to
##              1. set values of the matrix
##              2. get values of the matrix
##              3. set the inverse of the matrix
##              4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
## x is a square matrix
##              1. set values of the matrix
##              2. get values of the matrix
##              3. set the inverse of the matrix
##              4. get the inverse of the matrix

        invmatrix = NULL
        set = function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) invmatrix <<- inverse 
        getinv = function() invmatrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#################################################
#################################################

## Write a short comment describing this function
## @x: is the function from makeCacheMatrix()
## return: inverse of the original matrix in makeCacheMatrix()
##  It checks to see if the inversed nmatrix is cache already or not.  
##  If so, it uses the cached values and moves on.
##  If the cached values don't exist, then the inverse is calculated 
cacheSolve <- function(x, ...) {
        ## @x: is the function from makeCacheMatrix()
        ## return: inverse of the original matrix in makeCacheMatrix()
        
        invmatrix = x$getinv()
        
        # check to see if the inverse matrix exists 
        if (!is.null(invmatrix)){
                # get the matrix from the cache and skips the computation. 
                message("getting cached data")
                return(invmatrix)
        }
        # else, calculates the inverse matrix 
        mat.data = x$get()
        invmatrix = solve(mat.data, ...)
        
        # sets the value of the inverse matrix in cache using setinv.
        
        x$setinv(invmatrix)
        return(invmatrix)
}

