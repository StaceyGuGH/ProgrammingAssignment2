## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMtx <- x$getInverseMtx()
    if(!is.null(invMtx)) {
        message("getting cached data")
        return(invMtx)
    }
    data <- x$get()
    invMtx <- solve(data, ...)
    x$setInverseMtx(invMtx)
    invMtx
}
