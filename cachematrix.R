## For this assignment I will write two functions: 

## makeCacheMatrix which creates a caching of a solution and
## cacheSolve which works kind of like the singleton pattern:
## if there is no solution available it will create one. It will
## then return the current solution.

## creates a cached matrix which has two pieces of member data:
## a storedmatrix and a solution to the storedmatrix. When a new
## matrix is associated with the stored matrix we wipe the previous
## solution. I based this on the example of a cached vector provided
## but I really didn't like the variable names as they would be an
## issue for maintainability. I changed them for a more readable
## and maintainable implementation.

makeCacheMatrix <- function(newmatrix = matrix()) {
  ## member data for our cached matrix
  solution<<-NULL
  storedmatrix<<-newmatrix
  ## setter: stores a new matrix and wipes the old solution
  set <- function (newmatrix)
  {
    storedmatrix <<- newmatrix
    solution <<- NULL
  }
  
  ## getter: return the currently stored matrix
  get <- function() storedmatrix
  
  ## set a solution for the current stored matrix. I believe there
  ## is a bug here - it is returning the original matrix, not the 
  ## solution and I have no idea how to fix this.
  setsolve <- function(storedmatrix) solution <<-solve(storedmatrix)
  
  ## return the cached solution
  getsolve <-function() solution
  ## expose the operations available on this data. Note I find this
  ## to be not nearly as elegant as the object oriented approach
  list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}


## creates

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

