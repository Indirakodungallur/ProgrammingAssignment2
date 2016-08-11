## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special matrix object that can cache its inverse. This function
##returns a list containing functions to set and get matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x<<-y
    Inv <<- NULL
  }
  get<-function() x
  setInv <- function(inverse) Inv<<- inverse
  getInv <-function() Inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


## Write a short comment describing this function
## this function first checks if the inverse of the matrix is availble
## in cache. If yes then returns the same without calculating it again
## if not then calculates and sets in cache and returns the inverse.

cacheSolve <- function(x, ...) {
  Inv = x$getInv()
  if(!is.null(Inv)){
    message("getting cached inverse value")
    return(Inv)
  }
  data <-x$get()
  Inv<-solve(data)
  x$setInv(Inv)
  Inv
  ## Return a matrix that is the inverse of 'x'
}
