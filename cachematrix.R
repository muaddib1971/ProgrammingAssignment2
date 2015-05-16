## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(newmatrix = matrix()) {
  solution<<-NULL
  storedmatrix<<-newmatrix
  set <- function (newmatrix)
  {
    storedmatrix <<- newmatrix
    solution <<- NULL
  }
  get <- function() storedmatrix
  setsolve <- function(storedmatrix) solution <<-solve(storedmatrix)
  getsolve <-function() solution
  list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result<-x$getsolve()
  if(!is.null(result))
  {
    message("getting cached data")
    return (result)
  }
  data<-x$get()
  result<-solve(data, ...)
  x$setsolve(result)
  result
}

