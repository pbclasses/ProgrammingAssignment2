## Two functions makeCahceMatrix() and cacheSolve() allow for
## calculating and efficiently storing the inverse of a matrix.
## If a matrix inverse has not been determined, it is
## calculated and cached. If the matrix inverse has already
## been cached, that value is retrieved rather than 
## re-calculated.

## makeCacheMatrix returns a list of four functions.
## usage: mm = makeCacheMatrix(mdat)
## returns: mm$get(), mm$set(m), mm$getinv(), mm$setinv(inv)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix, either
## calculating it for the first time or a cached version
## usage: minv <- cacheSolve(mm)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
