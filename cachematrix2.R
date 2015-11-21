##These functions calculates and caches the inverse of a given matrix.
##This work is part of rprog-034 course in Coursera
##Elaborated by Daniele Bordalo


##Function: makeCacheMatrix
# This funtion creates a list containing functions to:
# a) set a given 'x' matrix,
# b) get the matrix,
# c) set inverse of the matrix and,
# d) get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



##Function: cacheSolve
#This function return a matrix that is the inverse of 'x' and tests existing conditions.
#Conditions tested: 
#a) non-square matrix,
#b) non reversible matrix and,
#c) and inverse with stored cache.
cacheSolve<- function(x, ...) {
  inv <- x$getInverse()
  test=ncol(x$get())-nrow(x$get())

  #exit tests
  if(test!=0) {
    return("must be a square matrix")
  }
  
  else {
    c3 <- det(x$get())
    if(c3==0) {
      return("not invertible")
    }}
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#testing a random matrix
a = replicate(5,rnorm(5)) 
a
b=makeCacheMatrix(a)
b$get()
b$getInverse()
cacheSolve(b)
cacheSolve(b)

#testing a non invertible matrix
a = matrix(1:16,4,4)
a
b=makeCacheMatrix(a)
b$get()
b$getInverse()
cacheSolve(b)
cacheSolve(b)

#testing a non square matrix
a<-matrix(c(3,5,6,7,8,9), ncol=3, nrow=2)
a
b=makeCacheMatrix(a)
b$get()
b$getInverse()
cacheSolve(b)
cacheSolve(b)


