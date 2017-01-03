## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  set<-function(y){
  x<<-y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
  mat<<-NULL  ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
}
get<-function() x
setmatrix<-function(solve) mat<<- solve
getmatrix<-function() mat
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    mat<-x$getmatrix()  # if an inverse has already been calculated this gets it
    if(!is.null(mat)){ # check to see if cacheSolve has been run before
      message("getting cached data")
      return(mat)
    }
    matrix<-x$get()
    mat<-solve(matrix, ...)# compute the value of the inverse of the input matrix
    x$setmatrix(mat)# run the setinverse function on the inverse to cache the inverse
    mat #returns inverse
}
