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


## essentially operates the way the singleton pattern works in 
## object oriented programming. We test if there is currently a 
## solution available and if there is we return it. Otherwise we
## calculate the solution and return that. It guarantees that we 
## only do the calculation once.

cacheSolve <- function(x, ...) {
  ## check for a previous solution
  result<-x$getsolve()
  if(!is.null(result))
  {
    ## if there is a previous solution, return it and exit this function
    message("getting cached data")
    return (result)
  }
  ## otherwise retrieve the matrix and perform solve on it
  data<-x$get()
  result<-solve(data, ...)
  ## save the solution for later use ... but this is not working correctly
  x$setsolve(result)
  ## return the result
  result
}

