## The functions provided allow for the calculation of an inverse matrix
## for any given matrix. To make future calculations more efficient, it 
## caches the results and so if the inverse matrix is requested again, the 
## function will simply return the cached inverse matrix, rather than calculating
## the inverse matrix anew.


## makeCacheMatrix takes a matrix as input and stores that matrix, along
## with four functions:
## set(y) - allows you to replace the stored matrix with the matrix 'y'
## get() - returns the stored matrix
## setInverse(inverse) - caches the inverse matrix 'inverse'
## getInverse() - returns the stored inverse matrix
makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    

}


## cacheSolve takes a makeCacheMatrix as an input and returns the inverse matrix.
## It checks whether the makeCacheMatrix input has a cached inverse matrix
## and returns that if it exists. Otherwise it calculates the inverse matrix
## anew and caches it in the makeCacheMatrix object for future requests.
cacheSolve <- function(x, ...) 
{
        
    i <- x$getInverse()
    
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setInverse(i)
    
    i
}
