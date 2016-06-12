## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse variable 
        i <- NULL
        
        ##Method to Set matrix method
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ##Method to get matrix method
        get <- function(){
                m
        }
        
        ##Method to set inverse of matrix method
        setInverseMatrix <- function(inverse) {
                i <<- inverse
        }
        
        ##Method to get inverse of matrix 
        getInverseMatrix <- function(){
                i
        }
        
        
        ## retunr a list of methods
        list(set = set, get = get, 
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        
        ## set Inverse value if it is defined or it is not null
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get Matrix object
        data <- x$get()
        
        ## Calculate inverse using matrix multipication
        m <- solve(data) %*% data
        
        ## set the inverse to the object
        x$setInverseMatrix(m)
        
        ## Return Matrix
        m
}
