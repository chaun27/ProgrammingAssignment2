## The following functions are used to create an enhanced matrix which caches
## the inverse of a mtrix after it is calculted.  This saves costly repeat calculations
##  when not needed.

## This function creates an inverse-caching matix from a regular matrix.
makeCacheMatrix <- function(x = matrix(x)) {
   inv <- NULL

   ## Overwrites the previous matrix with a newly supplied one and clears out 
   ## the cached inverse.
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }

   ## Returns the function.
   get <- function() x

   ## Calculates the inverse of the matrix
   setInverse <- function(solve) inv <<- solve
   
   ## Returns the cached inverse
   getInverse <- function() inv

   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## This function calculates and returns the inverse of an inverse-cahing matrix (defined above).
## If the inverse of the matrix has already been calculated, this function 
## returns the cached copy of the inverse. If a cached copy of the inverse is
## returned, a message indicating this fact is also displayed. 
cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
   
   ## If a cached copy of the inverse esists, tell the user and retun it.
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   ## Get the matrix
   data <- x$get()
   
   ## Calculate the invers
   inv <- solve(data, ...)
   x$setInverse(inv)
   
   ##Return the newly calculated inverse
   inv
}