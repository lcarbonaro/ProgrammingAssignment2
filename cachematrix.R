## These two functions create a matrix and calculate the inverse of
## that matrix respectively. The inverse is stored as a property of
## the first function, such that on subsequent calls to the second
## function this cached inverse is returned, rather than doing a 
## re-calculation of the inverse each time.


## This first function sets the matrix. It also stores
## the inverse of the matrix as a local property.
## Additionally, it provides getter/setter methods for both
## the inverse and the matrix itself.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL   ## inverse i.e. result of solve(x)
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) s <<- inverse
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the inverse of a matrix.
## It does this by first calling the getInverse() method 
## from the first function. 
## If the call to getInverse() returns a null, it means the
## inverse has not been calculated yet so far, in which case
## it is calculated, saved (setInverse) and returned.
## If the call to the getInverse() method returns an actual
## value (not null), then this is the cached inverse 
## and it is returned without the need to re-calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("Returning cached inverse")        
        } else {
                data <- x$get()
                s <- solve(data, ...) ## calc inverse
                x$setInverse(s) ## set inverse for future ref
        }
        s   ## return inverse
}
