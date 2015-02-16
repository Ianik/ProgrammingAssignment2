## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

## The matrix object has 2 fields x: the matrix and inv: it's inverse
## The object has as well 4 functions to access and set these fields:
## 1. "set" to set the value of the matrix
## 2. "get" to get the value of the matrix
## 3. "setinverse" to set the value of the inverse matrix
## 4. "getinverse" to get the value of the inverse matrix
## The assumption is that the x matrix is inversible. The function will test
## if it's a square matrix and give an error if not.

makeCacheMatrix <- function(x = matrix()) {
      ## Test is the matrix is a square Matrix
      if(!ncol(x)==nrow(x)){
            stop("The matrix is not a square matrix.")
      }
      inv <- NULL
      
      ## function to create new Matrix and set inverse field to NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## function that will return the matrix
      get <- function() x
      
      ## function that will set the inverse matrix
      setinverse <- function(inverse) inv <<- inverse
      
      ## function that will return the inverse Matrix
      getinverse <- function() inv
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## If the function is used with a different object that the special "matrix"
## it will return an error.
## The matrix should be inversible if not the solve function will return an error.
## The function will stop if the matrix is not square but it shouldn't have create
## it with the previous function.

## Result can be tested by: x$get() %*% x$getinverse()
## It will return the Identity matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## Test is the Matrix is a square Matrix. Shouldn't be needed 
      ## as it's checked in MakeMatrix and won't create object but just in case.
      if(!ncol(x$get())==nrow(x$get())){
            x$setinverse(NULL)
            stop("The matrix is not a square matrix.")
      }
      
      ## Get the inverse field from the object
      inv <- x$getinverse()
      
      ## if inverse field is already set return the inverse
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## getting inverse matrix from the solve function
      data <- x$get()
      inv <- solve(data, ...)
      
      ## set the inverse field in the object to the result of the solve function
      x$setinverse(inv)
      
      ## return the result
      inv
}
