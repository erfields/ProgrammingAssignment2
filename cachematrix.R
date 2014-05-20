## The makeCacheMatrix function matrix object as a parameter and caches its 
## inverse if the matrix is invertible.  The cacheSolve function returns the 
## cached inverse  matrix if the inverse has already been calculated and has 
## not changed.  Otherwise, the cacheSolve function computes the inverse of the 
## matrix returned by the makeCacheMatrix function.


## The makeCacheMatrix function creates a special "matrix" object that will 
## cache its inverse if an invertible matrix is passed as a parameter. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(mat1) m <<- solve(mat1)
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getcache()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- mat1(data, ...)
        x$setcache(m)
        m
}
