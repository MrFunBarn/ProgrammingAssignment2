## makeCacheMatrix and cacheSolve together allow for the a matrix to the users
## enviroment to have it's inverse calculated and then cached in the users
## enviroment for reuse with recalculating.

## makeCacheMatrix:
##     Creates a matrix object that can cache it's inverse into the
## calling enviroment for reuse.
## 
## Returns: A list of functions for setting and retriveing data between the calling
## and function enviroments.


makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  set <- function(y)
  {
      x   <<- y
      inv <<- NULL
  }

  get    <- function() x
  setinv <- function(invs) inv <<- invs
  getinv <- function() inv

  list( set    = set, 
        get    = get, 
        setinv = setinv, 
        getinv = getinv)
}


## cacheSolve:
##     Computes the inverse of a matrix given by makeCacheMatrix. If an inverse
##  is allready chached, returns that value, otherwise calculates, caches, and 
##  returns the inverse of x. Inverse calculated with solve().
##     
##  Returns: The inverse of the matrix x.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv
  
  if( !is.null(inv))
  {
      message("Getting chached Inverse") 
      return(inv)
  }
  
  data <- x$get()
  inv  <- solve(data)
  x$setinv(inv)
  
  inv
}