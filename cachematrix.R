## This pair of functions creates a special object that stores a matrix
## and caches its inverse. 

## The first function creates a special "matrix" object that can cache its inverse, which is really 
## a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s<-matrix()
  set<-function(y){
    x<<-y
    s<<-matrix()
  }
  get<-function()x
  setinverse<-function(solve) s<<-solve
  getinverse<-function() s
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the inverse 
## is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s<-x$getinverse()
  if(!is.na(s)){
    message("getting cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setinverse(s)
  s
  }
