## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inversem <- NULL
   set <- function(y) {
      x <<- y
      inversem <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inversem <<- inverse
   getinverse <- function() inversem
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The below function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  inversem <- x$getinverse()
  if(!is.null(inversem)) {
     message("getting cached data.")
     return(inversem)
  }
  data <- x$get()
  inversem <- solve(data)
  x$setinverse(inversem)
  inversem
}
