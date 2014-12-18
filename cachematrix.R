# The two functions below allow a user to calculate the inverse of a matrix
# and then to cache that value in the same function.
# Example:
# > m1 = matrix(c(1,0,0,1),nrow=2,ncol=2)
# > f1 = makeCacheMatrix(m1)
# > m2 = cacheSolve(f1)
# > m2 # See result
# > f1$setmatrix(m2)
# > m3 = cacheSolve(f1)
# > m3 

# The function makeCacheMatrix() gets and sets a matrix and its inverse.
# "This function creates a special "matrix" object that can cache its inverse."
# The matrix is passed in the "constructor" -- the matrix passed as an argument
# when the function is called -- or as an argument to the "method" setmatrix()
# that may be called to the function instance. The variables have the following
# roles:
# x is a matrix, passed to the outer "constructor" as an initial value
# y is a matrix, passed to the inner "method" setmatrix() that overwrites x and resets m
# i is the inverse of the matrix, stored in the outer closure for cacheing
# inverse is the argument passed to setinverse, i.e. the inverse is not calculated internally
# but by the second function, cacheSolve()
# Returns four "methods": setmatrix(y), getmatrix(), setinverse(mean), getinverse()
# All by setmatrix() are used by the second function, cacheSolve()
# Make sure the matrix is invertible!
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
# The function cacheSolve() takes the result of the first function as an argument
# and returns the inverse of the matrix that was passed to the first,
# either as the initial constructor or as an argument to $setmatrix.
# If the inverse has already been calculated and cached, the function
# returns the inverse. If not, it calculates, caches it, and then
# returns it.
cacheSolve <- function(f) {
    i <- f$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- f$getmatrix()
    i <- solve(data)
    f$setinverse(i)
    i
}