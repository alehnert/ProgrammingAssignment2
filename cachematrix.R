## R Programming: Second programming assignment
## created by alehnert, June 19, 2015
## contains two functions that (1) creates a special matrix that can cache its inverse and
## (2) computes the inverse of the special matrix if it does not already exist in cache

## Note: functions must be run in order, for example:
## > f1<-makeCacheMatrix(matrix(c(2,2,3,2),2,2))
## > cacheSolve(f1)  
## ---> returns inverse of matrix(c(2,2,3,2),2,2)
## > cacheSolve(f1)  
## ---> returns cached inverse of the matrix


## Creates a list of functions ("special matrix") that can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <-function()x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes the inverse of a matrix after checking if its inverse is cached
cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im  
}