## The functions provided here can cash the inverse of 
## a matrix, which is usually a costly computation

## "makeCacheMatrix" is used to set and get the value 
## of a matrix and its inverse respectively
#  test matrix:x<-matrix(c(1,2,3,4),nrow=2)
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## "cacheSolve" is used to calculates the inverse of the matix
## created by the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}

# verify the result:cacheSolve(makeCacheMatrix(x))
 