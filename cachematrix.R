##These functions: 
## -take a matrix as an input argument
## -check to see if the matrix inverse has been cached
##  to avoid calculating the inverse
## -invert the matrix
## -cache the matrix inverse
## -returns the matrix inverse


## This function:
## -clears the cache
## -creates a list of functions to 
##  1) reset the matrix and clear the cache
##  2) return the original matrix
##  3) cache the inverse
##  3) return the inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
        
    }
    getOrigMatrix <- function() x
    setInverseMatrix <- function(inv) i <<- inv
    getInverseMatrix <- function() i
    
    list(set = set,
         getOrigMatrix = getOrigMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)

}

## This function:
## -inverts a matrix, but first checks to see if the 
##  inverse has already been calculated and cached
## -if the inverse has not been cached, the function
##  then calculates the matrix inverse, then caches it
## and returns the inverse

cacheSolve <- function(x, ...) {
    i <- x$getInverseMatrix()
    if(!is.null(i)){
        message("Getting cached data")
        return (i)
    }
    matrix <- x$getOrigMatrix()
    i <- solve(matrix, ...)
    x$setInverseMatrix(i)
    i
        
}

a<-matrix(c(9,2,3,4,5,6,7,8,9), ncol=3)
x<-makeCacheMatrix(a)
cacheSolve(x)
cacheSolve(x)

b<-matrix(c(10,2,3,4,5,6,7,8,9), ncol=3)
x<-makeCacheMatrix(b)

cacheSolve(x)
cacheSolve(x)

