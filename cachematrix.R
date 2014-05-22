## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse

## NOTE: For this task, we are assuming the input matrix is inversible

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                ## very important to clear m when resetting x
                m <<- NULL
        }
        
        get <- function() x
        
        ## function definition for setinverse
        setinverse <- function(inverse) m <<- inverse
        
        ## function definition for getinverse
        getinverse <- function() m
        
        ## Return a list containing function names to be accessed
        ## from cacheSolve
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the mean of the special "vector" created 
## with makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.
## NOTE: For this task, we are assuming the input matrix is inversible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        ## Check if inverse is already calculated.
        ## If present, then return the cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get the input matrix
        data <- x$get()
        
        ## compute inverse of the matrix
        m <- solve(data, ...)
        
        ## set inverse value in cache
        x$setinverse(m)
        
        ## return inverse of the input matrix
        m
}
