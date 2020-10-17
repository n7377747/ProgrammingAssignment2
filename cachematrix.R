## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This special object as explained in the example encapsulates the data inside with four different functions
## two GETTERs for matrix and it's inverse and two SETTERs for the same

makeCacheMatrix <- function(x = matrix()) {
  x_inverse=NULL;
  set<-function(y){
    x<<-y
    x_inverse<<-NULL
  }
  set_inverse<-function(x_i)x_inverse<<-x_i
  get<-function()x
  get_inverse<-function()x_inverse
  list(set=set,
       get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## Write a short comment describing this function
## the function below is to retreive inverse of the matrix if present in the cache, otherwise calculate
cacheSolve <- function(x, ...) {
  x_inverse<-x$get_inverse()
  if(!is.null(x_inverse)){
    message("getting cached data")
    return(x_inverse)
  }
  data<-x$get()
  x_inverse<-solve(data,...)
  x$set_inverse(x_inverse)
  x_inverse
        ## Return a matrix that is the inverse of 'x'
}

