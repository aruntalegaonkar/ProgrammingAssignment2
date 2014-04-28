## This function  sets the value of the matrix, gets the value of the matrix.
## The function also sets the value of the matrix inverse and also gets the value of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
      x <<- y
      m <<- NULL
    }
    getmatrix <- function() x
    setinvmatrix <- function(invmatrix) m <<- invmatrix
    getinvmatrix <- function() m
    list(setmatrix=setmatrix,getmatrix=getmatrix,setinvmatrix=setinvmatrix,
         getinvmatrix=getinvmatrix)
}

## This checks if the matrix inverse is cached. If available the cached inverse
## is used. Otherwise the matrix inverse is calculated


cacheSolve <- function(x, ...) {
  m<-x$getinvmatrix()
  if(!is.null(m)){
    message("getting cached matrix inverse")
    return(m)
  }
  data <-x$getmatrix()
  m<-solve(data,...)
  x$setinvmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
