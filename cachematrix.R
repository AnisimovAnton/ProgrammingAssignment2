# These functions create a matrix object and calculate its inverse 
# caching it for further uses.

# makeCacheMatrix creates an object storing a matrix and its inverse.
# Takes a matrix as input.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    data <- NULL

    checkparam <- function (aParam) {
        if (is.matrix(aParam))
            aParam
        else {
            warning("cached matrix is initialized by non-matrix")
            NULL
        }
    }

    set <- function(aSet) {
        inv <<- NULL
        data <<- checkparam(aSet)
    }
    
    set(x)
    
    get <- function() data

    setinv <- function(aSetinv)
        inv <<- checkparam(aSetinv)
    
    getinv <- function() {
        if ( !is.null(inv) )
            message("getting cached inverse matrix value")
        else
            if ( !is.null(data) )
                inv <<- solve(data)
        inv
    }

    list (
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv )
}


# cacheSolve is an equvalent of solve function. If used 
# with an object created by a makeCacheMatrix it will cache 
# the result and return it in subsequent calls

cacheSolve <- function(x, ...) {    
    if (is.matrix(x))
        solve(x, ...)
    else if (is.list(x))
        x$getinv()
    else
        NA
}
