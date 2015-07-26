## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list containing a function to:
##
## set the value of the matrix
## get the value of the mtarix
## set the inverse of the matrix
## get the inverse of the matrix
##
##
#############################################################################
## Example used for testing: (Example credits : Paolo Di Lorenzo, from the course discussion page)
##
## > mat <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
## > print(mat)
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > matrixx <- makeCacheMatrix(mat)
## > matrixx$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > matrixx$getinverse()
## NULL
## > cacheSolve(matrixx)
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > matrixx$getinverse()
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > class(matrixx$get())
## [1] "matrix"
## > class(matrixx$getinverse())
## [1] "matrix"
##
#############################################################################

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseM <<- inverse
  getinverse <- function() inverseM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
