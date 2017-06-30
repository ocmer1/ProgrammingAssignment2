##makeCacheMatrix and cacheSolve are a set of functions that calculate the inverse of a matrix and store it in cache. If a matrix is new, it calculates the inverse, if it is already in cache, it returns the cached inverse. 


##makeCacheMatrix is an object that can set the value of a matrix and its inverse. If a new matrix is set, the inverse is cleared. It contains functions to get and set data, that are collected in a list. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Cachesolve is a function that checks whether the inverse is already present in the makeCacheMatrix object. If yes, it returns the inverse. If not, it calculates the inverse and sets in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
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
