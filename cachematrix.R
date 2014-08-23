## There are two functions in the file makeCacheMatrix and cacheSolve in which one creates the object and other is used to return Inverse of a matrix.

## makeCacheMatrix accepts a matrix and creates and returns a list object.
makeCacheMatrix <- function(x = matrix()) {#Accepts the parameter of Matrix type.
  
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,# List Object
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve can return cached or non-cached value of a matrix.
cacheSolve <- function(x, ...) { #Accepts the object created by makeCacheMatrix.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Returning cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}