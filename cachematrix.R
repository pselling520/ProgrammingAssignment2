## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as an input and creates a special form of the matrix
## storing the matrix and functions to use on the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #creating inverse
    
    #set matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #get matrix
    get <- function() x
    
    #set inverse
    setInverse <- function(inverse) i <<- inverse
    
    #get inverse
    getInverse <- function() i
    
    #return list containing the matrix and its inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This functions takes a special matrix created by them makeCacheMatrix function 
## as an input and outputs the inverse of that matrix. Before calculating the inverse,
## we check if the matrix has already been solved. Once solved, the inverse matrix 
## is cached so the calculations do not have to happen again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #look for existing inverse
    i <- x$getInverse()
    
    #if cached data exists, return inverse and leave function
    if(!is.null(i)){
        message <- "Getting cached data"
        return(i)
    }
    
    #If cached data doesn't exist, proceed with setting the inverse and return it
    matrix <- x$get()
    inverse <- solve(matrix)
    i <- x$setInverse(inverse)
    
    #Return inverse   
    i
}
 