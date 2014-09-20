# Two functions are included. The first one, 'makeCacheMatrix', enables you to 
# supply an invertible square matrix and cathe its inverse. In fact, what is returned by 'makeCacheMatrix' is a list of 
# functions. If you store the list in variable k, for example, in this way:
#
# k <- makeCacheMatrix(N)        ## (N is a variable that contains a matrix you want to calculate its inverse.)
#
# 'k$set(M)' returns a matrix M which updates the previous matrix N (N is now M).
# 'k$get()' returns the matrix N. If you used k$set(M) before, it returns M.
# 'k$setInv()' and 'k$getInv' are the same ones but for the inverse calculation.

# The second one, 'cacheSolve', calculates the inverse of the matrix given by 'makeCacheMatrix' (ex. 'cacheSolve(k)').
# If the inverse is already calculated, the function simply returns the inverse cached in 'makeCacheMatrix'.

# make a list of the functions about the matrix 
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
  
}


# This function computes the inverse of the special "matrix" returned by the first function.
# If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
  
}
