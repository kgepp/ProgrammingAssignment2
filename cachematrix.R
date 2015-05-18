## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())       # stores the cached value
{
        inverse_x<-NULL                         # initialize to NULL
        set<-function(y)                        # creates the matrix
        {
                x<<-y
                inverse_x<-NULL
        }
        get<-function()x                        # gets the value of the matrix
        setinverse<-function(inverse)           # inverts and stores the matrix in cache
                inverse_x<<-inverse
        getinverse<-function()                  # gets the inverted matrix from cache
                inverse_x
        list(set=set,get=get,                   # returns the functions
                setinverse=setinverse,
                getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        inverse_x<-x$getinverse()               # returns the matrix if it exists in cache
        if(!is.null(inverse_x))
        {
                message("getting inversed matrix from cache")
                return(inverse_x)
        }
        else                                    # creates the matrix if it doesn't exist in cache
        {
                inverse_x<-solve(x$get())
                x$setinverse(inverse_x)
                return(inverse_x)               ## Return a matrix that is the inverse of 'x'
        }
}
