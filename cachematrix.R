## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               # Establish m in makeCacheMatrix's environment. Otherwise m would have define m globally. Resets the process.
        set <- function(y) {                    # The set function ensures when x is changes the cache is flushed Provides ability to change matrix after creation, resetting the original vector.
                x <<- y
                m <<- NULL                      # But if you do, set m flag to indicate inverse? matrix has not been cached. Flush the inverse.
        }
        get <- function() x                     # Returns the entry matrix (?defined in global environment?)
        setinverse <- function(inverse) m <<- inverse  # Returns the value for m.Assign the argument to m. Makes this available outside the function. Thiw will eventually be the inverted matrix created by solve. Place in parent environment to cache the inverted matrix. Cause R to look for 'm' in the parent environment of setinverse. It finds it makeCacheMatrix's enviroment, and sets it to the parametrs passed to the setinverse function.
        getinverse <- function() m              # getinverse checks to see if m has been populate in parent enviornment, populated by set inverse in the past.
        list(set = set, get = get,              # Return four functions as a list, with their environments. Each element in the list is named with function names.
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                     # First set m to the existing value returned from getinverse function above
        if(!is.null(m)) {                       # Check if inverted matrix has been already cached. If not NIULL there is a value stored in m so return it
                message("getting cached data")  # If so, provide message about cache, and
                return(m)                       # Return the cached inverted matrix and stop
        }
        data <- x$get()                         # If m not set, get the matrix to be inverted 
        m <- solve(data, ...)                   # Invert the matrix using solve function
        x$setinverse(m)                         # Place in parent environment to cache the inverted matrix
        m                                       # Output the value of m                              
}