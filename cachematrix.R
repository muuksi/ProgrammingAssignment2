## Two functions to inverse a matrix object x
## the functions also trys to save time by storing the inversed matrix in cache
## and returning the cached matrix if present.

## The first function makeCacheMatrix gets and sets the Matrix object in the 
## cache or retrieves the matrix from the cache.
## The second function inverses a matrix object and checks if its already 
## in the cache before returning a inversed matrix.

## makeCacheMatrix
## handles the cacheed matrix with the SET and GET function
makeCacheMatrix <- function(x = matrix()) {
        tempMatrix <- NULL
        ## adding the matrix to the Cache
        set <- function(myMatrix){
                x <- myMatrix
                tempMatrix <- NULL
        }
        ## the get function retreives the matrix
        get <- function() x
        
        ## the setInverse function stores the matrix in the cache
        setInverse <- function(myInverse) tempMatrix <<- myInverse  
        ## the getInverse gets the matrix from the cache
        getInverse <- function() tempMatrix
        
        # defining getters and setters
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}



## cacheSolve 
## takes the input matrix x
## passes x to function makeCacheMatrix to do so
## ckecking if cache is already present, if so return cache
## else, pass matrix to inverse it and store it in the cache
## returning the inversed matrix
cacheSolve <- function(x, ...) {
        # try to get the cached inverse matrix
        tempMatrix <- x$getInverse()
        # is the cache available?
        if (!is.null(tempMatrix)){
                # returns the cached matrix
                return (tempMatrix)
        }
        # get the matrix object x
        data <- x$get()
        # inversing the matrix object data with solve
        tempMatrix <- solve(data, ...)
        # set the inversed matrix in the cache for later
        x$setInverse(tempMatrix)
        # return the inversed tempMatrix
        tempMatrix
}
