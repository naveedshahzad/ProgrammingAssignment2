## This file contains a pair of functions that cache the inverse of a matrix.

#create a cacheable matrix
#example
#m<-matrix(1:4, 2,2)
#n<-makeCacheMatrix(x=m)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
#Will compute inverse of given matrix created by makeCacheMatrix function
#if inverse of the matrix already computed, will return it from cache without computing.
#otherwise compute the inverse and store it in the cache
#example
#
#m<-matrix(1:4, 2,2)
#m is assumed to be square and invertible here
#n<-makeCacheMatrix(x=m)
#p=cacheSolve(n)
#to verify if the calculated value is correct
#round(n$get() %*% p) should return an identity matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
