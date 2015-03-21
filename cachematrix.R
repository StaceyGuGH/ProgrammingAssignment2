
##Caching the Inverse of a Matrix
##   Matrix inversion is usually a costly computation and there may be some 
##   benefit to cache the value of the inverse matrix so that when we need it 
##   again, it can be looked up in the cache rather than recomputed.
##
##   The following 2 functions take advantage of the scoping rules of the R 
##   language and the "<<-" operator which can be used to assign a value to an 
##   object in an environment that is different from the current environment.
##   See help ("<<-") for more details on "<<-".
##
##To test the following 2 functions, try
##   m <- matrix(rnorm(9), 3,3)
##   x <- makeCacheMatrix(m)
##   cacheSolve(x)
##   cacheSolve(x)


##Function "makeCacheMatrix" creates a special "matrix" object that can ...
##cache its inverse. The special "matrix" is a list of functions to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invMtx <- NULL
    set <- function(y) {
        x <<- y
        invMtx <<- NULL
    }
    get <- function() x
    setInverseMtx <- function(tmpX) invMtx <<- tmpX
    getInverseMtx <- function() invMtx
    list(set = set, get = get,
         setInverseMtx = setInverseMtx,
         getInverseMtx = getInverseMtx)
}



##Function "cacheSolve" calculates the inverse of the special "matrix" ... 
##created with function "makeCacheMatrix". 
##       It first checks to see if the inverse has already been calculated ...
##          via the getInverseMtx function.
##       If so, it sets the inverse from the cache ...
##           and skips the computation. 
##       Otherwise, it calculates the inverse of the matrix and ...
##           sets the value of the inverse in the cache via ...
##           the setInverseMtx function.
## Assumption: the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMtx <- x$getInverseMtx()
    if(!is.null(invMtx)) {
        message("getting cached data")
        return(invMtx)
    }
    data <- x$get()
    ## Use the "solve" function in R to Compute the inverse of a square matrix
    invMtx <- solve(data, ...)
    x$setInverseMtx(invMtx)
    invMtx
}
