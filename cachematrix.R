## This small bit of code allows us to cache and get the results of a matrix that has previously
## been inverted ; if the inverted value invM has not previously been computed, it will compute it 
## using the solve function and return it for us.

## This function allows us to create a version of our original matrix as a list which also contains
## the necessary set and get functions.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
      x <<- y
      invM <<- NULL
    }
    get <- function() x
    setinvM <- function(solve) invM <<- solve
    getinvM <- function() invM
    list(set = set, get = get,
         setinvM = setinvM,
         getinvM = getinvM)
  }

## This function returns the value for invM : if it has been previously computed, it simply gets it 
## back (and tells us so!), if it has not, it computes it and prints it for us.

cacheSolve <- function(x, ...) {
  invM <- x$getinvM()
  if(!is.null(invM)) {
    message("Getting cached data values :")
    return(invM)
  }
  data <- x$get()
  InvM <- solve(data, ...)
  x$setinvM(invM)
  InvM
}
        ## Return a matrix that is the inverse of 'x'

## First we need to create a copy of x using the first function :

z <- makeCacheMatrix(x)

## Then we execute the second function on z, in order to get our inverted matrix :

cacheSolve(z)

## If you want to check that the code works, I have created a small invertible matrix as an example :

a <- cbind(c(1,7,8),c(0,2,4),c(3,2,12))
z <- makeCacheMatrix(a)
cacheSolve(z)

## Which should return the same result as

solve(a)