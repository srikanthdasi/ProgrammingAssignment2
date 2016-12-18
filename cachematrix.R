##Pragramming Assigmnet on Lexical Scoping

## makeCacheMatrix: This function creates a special 
##     "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvers <- function(inverse) inv <<- inverse
  getInvers <- function() inv
  list(set = set,
       get = get,
       setInvers = setInvers,
       getInvers = getInvers)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             inv <- x$getInvers()
     if (!is.null(inv)) {
       message("getting cached data")
     return(inv)   ##inv matrix that is the inverse of 'x'
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInvers(inv)
   inv
}
