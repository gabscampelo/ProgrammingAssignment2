#Caching the inverse of a matrix
        # Two functions are defined to cache the inverse of a matrix, and avoid computing it repeatedly.
# Function 1: "makeCacheMatrix"
        # Creates a matrix object that can cache its inverse
# Function 2: "cacheSolve"
        # Computes the inverse of the matrix returned by "makeCacheMatrix". 
        # If the inverse has already been calculated (and the matrix has not changed),
        # then "cacheSolve" retrieves the inverse from the cached matrix. 


# makeCacheMatrix
# Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve
# Computes the inverse of the matrix returned by "makeCacheMatrix".
cacheSolve <- function(x, ...) {
    i <<- x$getinv()
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
