## Solution Task 2 of course Courser R Programming
## - It solves the inverse matrix with optimization to
## frequent calculation over the same matrices, ie.
## containing cacheing.



## This function creates a special ("matrix") object
## that can cache (its inverse).
##
## 'x' is a numeric matrix or something
##
## Return a access functions to 'x' and 'cache'
## a <- makeCacheMatrix(x)
## a$set(x1),  a$get(), a$setCache(x2), a$getCache()
##
makeCacheMatrix <- function(x = matrix()) {
  # init internal cache, access functions
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(cache) m <<- cache
  getCache <- function() m
  
  # result list of access functions
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}



## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## 'x' is a "numeric matrix" created makeChaceMatrix(...)
##
## Return:
## - if empty cache than computed inverse matrix for x -> x$getCache()
## - return inverse matrix from x$get()
##
cacheSolve <- function(x, ...) {
  # check existing computed inverse matrix
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # compute inverse matrix and save to cache
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  
  # return inverse (new) matrix
  m
}
