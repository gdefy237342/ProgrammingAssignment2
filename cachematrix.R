## This R function pair may be used to cache the inverse of a matrix and then
## look it up later. Caching saves you having to calculate the inverse of the
## matrix again, a potentially time consuming operation, when the matrix has not
## changed. It is assummed that the matrix is invertible. The functions make use
## of R's lexical scoping rules to preserve state. This R code is based on Roger
## Peng's example for caching the mean of a vector at
## https://github.com/rdpeng/ProgrammingAssignment2 .

## The makeCacheMatrix function creates the special object that can be used to
## cache the inverse of the matrix and also provides the means to manipulate it.

makeCacheMatrix <- function(x = matrix()) {
        ## Establish i in makeCacheMatrix's environment, otherwise the globally 
        ## defined i would be used.
        i <- NULL
        ## The set function sets the value of the matrix. It also provides the
        ## ability to change the matrix after creation, loading a new one. When
        ## x is changed the cache of the inverted matrix is flushed.
        set <- function(y) {
                ## Assign set's argument to x in the parent environment, that is
                ## different from the current environment of set.
                x <<- y
                ## As the matrix has been set (or changed), flush the cache of
                ## the inverted matrix. Assign value to i in the parent
                ## environment (different to the current environment of set).
                i <<- NULL
        }
        ## The get function returns the matrix that has been set (and defined in
        ## the parent environment).
        get <- function() x
        ## The setinverse function assigns the paramters passed to it to i, the
        ## inverted matrix created by solve. i is in a different environment to
        ## setinverse's environment; this makes i available outside the
        ## function. R looks in the parent environment of setinverse, finding i
        ## in makeCacheMatrix's environment.
        setinverse <- function(inverse) i <<- inverse
        ## The getinverse function returns the object that holds the inverted
        ## matrix. This could be null if the inverted matrix has not yet been
        ## calculated.
        getinverse <- function() i
        ## The special object returned by makeCacheMatrix is a list containing
        ## four functions to set the matrix, get the matrix, set the inverse of
        ## the matrix and get the inverted matrix. Each function is returned with
        ## its environment. Each element in the list is named with the function
        ## names for easy reference.
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special object created
## by the makeCacheMatrix function. It first checks to see if cache containing
## the inverted matrix has been set; if so, it gets the inverse of the
## matrix from the cache and does not need to recalculate it. Otherwise it
## calculates the inverse of the matrix and sets the cache.

cacheSolve <- function(x, ...) {
        ## Get the inverted matrix for the x matrix, using getinverse function
        ## from the makeCacheMatrix above. The inverted matrix is cached with
        ## the aid of lexical scoping, with an environment created for each
        ## use of makeCacheMatrix.
        i <- x$getinverse()
        ## Check if an inverted matrix has been already cached. If i is NULL
        ## there is a value stored in i so return it, skipping the calculation.
        if(!is.null(i)) {
                ## As cache has been loaded, provide message about cache. 
                message("getting cached data")
                ## Return the cached inverted matrix and stop.
                return(i)
        }
        ## As there is no cached matrix, get the matrix to be inverted. 
        data <- x$get()
        ## Invert the matrix using solve function (note it is assumed that x is
        ## a square invertible matrix).
        i <- solve(data, ...)
        ## Use setinverse to cache the inverted matrix result and place in
        ## parent environment.
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x' 
        i                             
}