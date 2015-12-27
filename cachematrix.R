## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## A new R object is created from a given matrix which contains the 
## matrix and can contain its inverse as well

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve(x)
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)

}

## cacheSolve
## checks to see if the inverse of the matrix has already been calculated.
## If so the cached inverse matrix is returned
## If not, the inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
