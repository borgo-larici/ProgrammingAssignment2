## The two functions below demonstrate how the scoping rules of the R language
## let you preserve state inside of an R object.  This is useful for caching
## the results of expensive computations.  The cached result can be retrieved
## instead of performing the computation anew.

## This function creates a special "matrix wrapper" object that knows how
## to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Wipe the x.inverse object that holds the inverse of the matrix
        x.inverse <- NULL
        
        ## Define the "set" function.  It assigns the passed object "y"
        ## to the parent's persisted (cached) object "x", then wipes the 
        ## parent's persisted (cached) "x.inverse".
        set <- function(y) {
                ## Assign y to parent's x
                x <<- y
                ## wipe parent's x.inverse
                x.inverse <<- NULL
        }
        
        ## Define the "get" function.  It returns the parent's "x" since no
        ## "x" is defined in the "get" function's closure.
        get <- function() x
        
        ## Define the "setinverse" function.  It assigned the value of the 
        ## passed argument to the parent's persisted "x.inverse" object.
        setinverse <- function(inv) x.inverse <<- inv
        
        ## Define the "getinverse" function.  It returns the parent's
        ## persisted "x.inverse"
        getinverse <- function() x.inverse
        
        ## Return a list of four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix wrapper" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve returns the pre-calculated inverse
## from the cache.


cacheSolve <- function(x) {
        
        ## Put the cached inverse into "xi"
        xi <- x$getinverse()
        
        ## Is "xi" null?
        if(!is.null(xi)) {
                ## "xi" is not null.
                ## Tell the user we're returning the cached value.
                message("getting cached data")
                ## Return the cached value
                return(xi)
        }
        
        ## Execution continues here if "xi" is null.
        
        ## Get the matrix that was cached in the passed object
        data <- x$get()
        
        ## Solve the matrix into "xi"
        xi <- solve(data)
        
        ## Persist the solved (inverted) matrix
        x$setinverse(xi)
        
        ## Return the inverted matrix
        xi
}
