## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. The functions provided here
## will cache the inverese of a matrix.


## This function a) sets the value of a matrix, b) gets the value of a matrix, 
## c) sets the inverse of the matrix, d) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## This function calculates the inverse of the matrix, but checks first whether it was calculated already
## and if so gets it from the cache

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <-x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
